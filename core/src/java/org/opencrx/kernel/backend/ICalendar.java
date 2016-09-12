/*
 * ====================================================================
 * Project:     openCRX/Core, http://www.opencrx.org/
 * Description: ICalendar
 * Owner:       CRIXP AG, Switzerland, http://www.crixp.com
 * ====================================================================
 *
 * This software is published under the BSD license
 * as listed below.
 * 
 * Copyright (c) 2004-2012, CRIXP Corp., Switzerland
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
package org.opencrx.kernel.backend;

import java.io.BufferedReader;
import java.io.ByteArrayInputStream;
import java.io.IOException;
import java.io.InputStream;
import java.io.InputStreamReader;
import java.io.OutputStreamWriter;
import java.io.PrintWriter;
import java.io.StringReader;
import java.io.UnsupportedEncodingException;
import java.text.ParseException;
import java.text.SimpleDateFormat;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.Calendar;
import java.util.Collection;
import java.util.Collections;
import java.util.Date;
import java.util.GregorianCalendar;
import java.util.HashMap;
import java.util.Iterator;
import java.util.List;
import java.util.Map;
import java.util.TimeZone;
import java.util.UUID;
import java.util.logging.Level;

import javax.jdo.JDOHelper;
import javax.jdo.PersistenceManager;
import javax.mail.internet.InternetAddress;
import javax.servlet.http.HttpServletRequest;

import org.opencrx.kernel.account1.jmi1.Account;
import org.opencrx.kernel.account1.jmi1.Contact;
import org.opencrx.kernel.account1.jmi1.EMailAddress;
import org.opencrx.kernel.activity1.cci2.ActivityQuery;
import org.opencrx.kernel.activity1.cci2.EMailRecipientQuery;
import org.opencrx.kernel.activity1.cci2.IncidentPartyQuery;
import org.opencrx.kernel.activity1.cci2.MeetingPartyQuery;
import org.opencrx.kernel.activity1.cci2.TaskPartyQuery;
import org.opencrx.kernel.activity1.jmi1.Absence;
import org.opencrx.kernel.activity1.jmi1.AbstractActivityParty;
import org.opencrx.kernel.activity1.jmi1.AbstractFilterActivity;
import org.opencrx.kernel.activity1.jmi1.Activity;
import org.opencrx.kernel.activity1.jmi1.ActivityCreator;
import org.opencrx.kernel.activity1.jmi1.ActivityGroup;
import org.opencrx.kernel.activity1.jmi1.EMail;
import org.opencrx.kernel.activity1.jmi1.EMailRecipient;
import org.opencrx.kernel.activity1.jmi1.ExternalActivity;
import org.opencrx.kernel.activity1.jmi1.Incident;
import org.opencrx.kernel.activity1.jmi1.IncidentParty;
import org.opencrx.kernel.activity1.jmi1.Mailing;
import org.opencrx.kernel.activity1.jmi1.Meeting;
import org.opencrx.kernel.activity1.jmi1.MeetingParty;
import org.opencrx.kernel.activity1.jmi1.NewActivityParams;
import org.opencrx.kernel.activity1.jmi1.NewActivityResult;
import org.opencrx.kernel.activity1.jmi1.PhoneCall;
import org.opencrx.kernel.activity1.jmi1.Resource;
import org.opencrx.kernel.activity1.jmi1.Task;
import org.opencrx.kernel.activity1.jmi1.TaskParty;
import org.opencrx.kernel.backend.Activities.ActivityClass;
import org.opencrx.kernel.backend.Activities.ActivityState;
import org.opencrx.kernel.backend.Activities.PartyStatus;
import org.opencrx.kernel.backend.Activities.PartyType;
import org.opencrx.kernel.base.jmi1.ImportParams;
import org.opencrx.kernel.generic.SecurityKeys;
import org.opencrx.kernel.home1.cci2.TimerQuery;
import org.opencrx.kernel.home1.jmi1.Timer;
import org.opencrx.kernel.home1.jmi1.UserHome;
import org.opencrx.kernel.utils.ActivityQueryHelper;
import org.opencrx.kernel.utils.Utils;
import org.openmdx.base.accessor.jmi.cci.RefObject_1_0;
import org.openmdx.base.exception.ServiceException;
import org.openmdx.base.io.QuotaByteArrayOutputStream;
import org.openmdx.base.jmi1.BasicObject;
import org.openmdx.base.text.conversion.UUIDConversion;
import org.openmdx.kernel.exception.BasicException;
import org.openmdx.kernel.id.UUIDs;
import org.openmdx.kernel.log.SysLog;
import org.w3c.format.DateTimeFormat;
import org.w3c.spi2.Datatypes;
import org.w3c.spi2.Structures;

/**
 * ICalendar
 *
 */
public class ICalendar extends AbstractImpl {

	/**
	 * Register ICalendar backend implementation.
	 */
	public static void register(
	) {
		registerImpl(new ICalendar());
	}
	
	/**
	 * Get instance of registered ICalendar backend implementation.
	 * 
	 * @return
	 * @throws ServiceException
	 */
	public static ICalendar getInstance(		
	) throws ServiceException {
		return getInstance(ICalendar.class);
	}

	/**
	 * Constructor.
	 */
	protected ICalendar(
	) {
		
	}
	
	/**
	 * ICalField
	 *
	 */
	public static class ICalField {
		
		public ICalField(
			String name,
			String value,
			Map<String,List<String>> parameters
		) {
			this.name = name;
			this.value = value;
			this.parameters = parameters;
		}

		public static ICalField getField(
			String name,
			String parameterName,
			List<String> parameterValues,
			Map<String,ICalField> ical
		) {
			List<ICalField> matchingFields = ICalField.findFields(name, parameterName, parameterValues, ical);
			return matchingFields.isEmpty() ?
				null :
					matchingFields.get(0);
		}

		public static List<ICalField> findFields(
			String name,
			String parameterName,
			List<String> parameterValues,			
			Map<String,ICalField> ical
		) {
			List<ICalField> matchingFields = new ArrayList<ICalField>();
			for(ICalField field: ical.values()) {
				if(
					name.equals(field.getName()) && 
					(parameterName == null || (field.getParameters().get(parameterName) != null &&
					field.getParameters().get(parameterName).containsAll(parameterValues)))
				) {
					matchingFields.add(field);
				}
			}
			return matchingFields;
		}
		
		public static String getFieldValue(
			String name,
			String parameterName,
			List<String> parameterValues,
			Map<String,ICalField> ical			
		) {
			ICalField field = getField(
				name,
				parameterName,
				parameterValues,
				ical
			);
			return field == null ? null : field.getValue();
		}
		
		public static String getFieldValue(
			String name,
			Map<String,ICalField> ical			
		) {
			return getFieldValue(
				name,
				null,
				null,
				ical
			);
		}
		
	    public TimeZone getTimeZone(
	    ) {
	        List<String> tzids = this.getParameters().get("TZID");
	        TimeZone tz = TimeZone.getDefault();
	        if(tzids != null && !tzids.isEmpty()) {
	        	String tzid = tzids.get(0).toUpperCase();
	            String[] availableIds = TimeZone.getAvailableIDs();
	            for(int i = 0; i < availableIds.length; i++) {
	                if(tzid.endsWith(availableIds[i].toUpperCase())) {
	                    tz = TimeZone.getTimeZone(availableIds[i]);
	                    break;
	                }
	            }
	        }
	        return tz;
	    }
		
		public String getName(
		) {
			return this.name;
		}
		
		public String getValue(
		) {
			return this.value;
		}
		
		public void setValue(
			String value
		) {
			this.value = value;
		}
		
		public Map<String,List<String>> getParameters(
		) {
			return this.parameters;
		}

		@Override
        public boolean equals(
        	Object obj
        ) {
			if(obj instanceof ICalField) {
				ICalField that = (ICalField)obj;
				return 
					this.name.equals(that.name) && 
					this.parameters.equals(that.parameters) &&
					this.value.equals(that.value);
			} else {
				return super.equals(obj);
			}
        }
		
		@Override
        public String toString(
        ) {
			return this.name + this.parameters.toString() + ":" + this.value;
        }

		private final String name;
		private String value;
		private final Map<String,List<String>> parameters;
	}
	
    /**
     * Replace all control characters by blanks except newline. 
     * The newline \n is escaped and replaced by '\n'.
     * 
     * @param from
     * @return
     */
    private String escapeControlChars(
        String from
    ) {
    	if(from == null) return null;
    	String to = "";
    	for(int i = 0; i < from.length(); i++) {
    		char c = from.charAt(i);
    		if(c == '\n') {
    			to += "\\n";
    		} else if(!Character.isValidCodePoint(c) || c < ' ') {
    			to += " ";
    		} else {
    			to += c;
    		}
    	}
    	return to;
    }

    /**
     * Escape commas by '\,'. The input is unescaped.
     * 
     * @param from
     * @return
     */
    private String escapeCommas(
        String from
    ) {
    	return from.replace("\\,", ",").replace(",", "\\,");
    }

    /**
     * Get value for flag X-OPENCRX-RENDER-ALARMS. By default the method
     * returns true if the activity has assigned timers. Override this
     * method for custom-specific behavior.
     * 
     * @param activity
     * @return
     */
    protected boolean isFlagRenderAlarms(
    	Activity activity
    ) {
    	return 
    		!JDOHelper.isNew(activity) && 
    		!activity.getAssignedTimer().isEmpty();
    }

    /**
     * Merge fields of activity's ical with supplied ical.
     * 
     * @param activity
     * @param sourceIcal
     * @param statusMessage
     * @return
     * @throws ServiceException
     */
    public String mergeIcal(
        Activity activity,
        String sourceIcal,
        List<String> statusMessage
    ) throws ServiceException {
    	PersistenceManager pm = JDOHelper.getPersistenceManager(activity);    	
        short icalType = activity.getIcalType();
        if(icalType == ICAL_TYPE_NA) {
        	// Try to derive get icalType from sourceICal
        	if((sourceIcal != null) && (sourceIcal.indexOf("BEGIN:VEVENT") > 0)) {
        		icalType = ICAL_TYPE_VEVENT;
        	} else if((sourceIcal != null) && (sourceIcal.indexOf("BEGIN:VTODO") > 0)) {
        		icalType = ICAL_TYPE_VTODO;
        	}
        	// If still undefined, try to derive from last applied creator
        	else {
	            ActivityCreator activityCreator = null;
	            try {
	            	activityCreator = activity.getLastAppliedCreator();
	            } catch(Exception e) {} // don't care if creator can not be accessed
	            if(
	            	(activityCreator != null) && 
	            	(activityCreator.getIcalType() != ICAL_TYPE_NA)
	            ) {
	                icalType = activityCreator.getIcalType();
	            } else {
	            	if(activity instanceof Absence) {
	            		icalType = ICAL_TYPE_VEVENT;
	            	} else if(activity instanceof EMail) {
	            		icalType = ICAL_TYPE_VEVENT;
	            	} else if(activity instanceof ExternalActivity) {
	            		icalType = ICAL_TYPE_VEVENT;
	            	} else if(activity instanceof Incident) {
	            		icalType = ICAL_TYPE_VTODO;
	            	} else if(activity instanceof Mailing) {
	            		icalType = ICAL_TYPE_VEVENT;
	            	} else if(activity instanceof Meeting) {
	            		icalType = ICAL_TYPE_VEVENT;
	            	} else if(activity instanceof PhoneCall) {
	            		icalType = ICAL_TYPE_VEVENT;
	            	} else if(activity instanceof Task) {
	            		icalType = ICAL_TYPE_VTODO;
	            	}
	            }
        	}
        }
        // DTSTART
        String dtStart = null;
        if(activity.getScheduledStart() != null) {
            dtStart = DateTimeFormat.BASIC_UTC_FORMAT.format(activity.getScheduledStart());
        } else {
            // ical requires DTSTART. Set DTSTART to beginning of year 
            // if scheduledStart is not defined for activity
            GregorianCalendar cal = new GregorianCalendar();
            dtStart = cal.get(Calendar.YEAR) + "0101T000000.000Z";
        }
        // DTEND
        String dtEnd = null;
        if(activity.getScheduledEnd() != null) {
            dtEnd = DateTimeFormat.BASIC_UTC_FORMAT.format(activity.getScheduledEnd());
        }
        // DUE
        String dueBy = null;
        if(activity.getDueBy() != null) {
            dueBy = DateTimeFormat.BASIC_UTC_FORMAT.format(activity.getDueBy());
        }
        // COMPLETED
        String completed = null;
        if(activity.getActualEnd() != null) {
            completed = DateTimeFormat.BASIC_UTC_FORMAT.format(activity.getActualEnd());
        }
        // LAST-MODIFIED
        String lastModified = DateTimeFormat.BASIC_UTC_FORMAT.format(new Date());
        // PRIORITY
        Number priority = activity.getPriority();
        // SUMMARY
        String summary = activity.getName() == null 
        	? "" 
        	: activity.getName();
        // append activity number as comments
        summary += activity.getActivityNumber() == null 
        	? "" 
        	: Base.COMMENT_SEPARATOR_EOT + " #" + activity.getActivityNumber();
        // append misc1 if it is of the form [...]
        String misc1 = activity.getMisc1();
        if(misc1 != null && misc1.startsWith("[") && misc1.endsWith("]")) {
        	summary += " " + misc1;
        }
        // DESCRIPTION
        String description = activity.getDescription() == null 
        	? "" 
        		: activity.getDescription();
        // LOCATION
        String location = activity.getLocation() == null ? "" : 
            activity.getLocation();
        // STATUS
        String status = null;
        // CLASS
        ICalClass icalClass = ICalClass.valueOf(activity.getIcalClass());        
        // VTODO
        if(icalType == ICAL_TYPE_VTODO) {
            Number percentComplete = activity.getPercentComplete();
            if(Boolean.TRUE.equals(activity.isDisabled())) {
                status = "CANCELLED";
            } else if(percentComplete == null || percentComplete.intValue() == 0) {
            	status = "NEEDS-ACTION";
            } else if(percentComplete != null && percentComplete.intValue() >= 100) {
            	status = activity.getActivityState() == ActivityState.CANCELLED.getValue() ?
            		"CANCELLED" :
            			"COMPLETED";
            } else {
                status = "IN-PROCESS";
            }
        } else {
            // VEVENT
            Number percentComplete = activity.getPercentComplete();
            if(Boolean.TRUE.equals(activity.isDisabled())) {
                status = "CANCELLED";
            } else if(percentComplete == null || percentComplete.intValue() == 0) {
                status = "TENTATIVE";
            } else if(percentComplete != null && percentComplete.intValue() < 100) {
                status = "CONFIRMED";
            } else {
            	status = activity.getActivityState() == ActivityState.CANCELLED.getValue() ?
            		"CANCELLED" :
            			"COMPLETED";                	
            }
        }
        // Attendees
        List<String> attendees = new ArrayList<String>();
        List<AbstractActivityParty> parties = Activities.getInstance().getActivityParties(activity); 
        String organizerEMail = null;
        for(AbstractActivityParty party: parties) {
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
                    if(emailAddress != null) {
                    	if(
                    		party.getPartyType() == PartyType.ORGANIZER.getValue() ||
                    		party.getPartyType() == PartyType.EMAIL_FROM.getValue()
                    	) {
                    		organizerEMail = emailAddress;
                    	} else {
		                    String partyType = null;
		                    if(
		                    	party.getPartyType() == PartyType.OPTIONAL.getValue() ||
		                    	party.getPartyType() == PartyType.EMAIL_CC.getValue()
		                    ) {
	                    		partyType = "OPT-PARTICIPANT";
		                    } else if(
		                    	party.getPartyType() == PartyType.REQUIRED.getValue() || 
		                    	party.getPartyType() == PartyType.EMAIL_TO.getValue()		                    	
		                    ) {
	                    		partyType = "REQ-PARTICIPANT";
		                    } else if(party.getPartyType() == PartyType.EMAIL_BCC.getValue()) {
		                    	// do not add as attendee
		                    } else {
			                    partyType = "NON-PARTICIPANT";
		                    }
		                    if(partyType != null) {
			                    String partyStatus = null;
			                    if(party.getPartyStatus() == PartyStatus.NEEDS_ACTION.getValue()) {
			                    	partyStatus = "NEEDS-ACTION";
			                    } else if(party.getPartyStatus() == PartyStatus.ACCEPTED.getValue()) {
		                    		partyStatus = "ACCEPTED";
			                    } else if(party.getPartyStatus() == PartyStatus.DECLINED.getValue()) {
		                    		partyStatus = "DECLINED";
			                    } else if(party.getPartyStatus() == PartyStatus.TENTATIVE.getValue()) {
		                    		partyStatus = "TENTATIVE";
			                    } else if(party.getPartyStatus() == PartyStatus.DELEGATED.getValue()) {
		                    		partyStatus = "DELEGATED";
			                    } else if(party.getPartyStatus() == PartyStatus.COMPLETED.getValue()) {
		                    		partyStatus = "COMPLETED";
			                    }
		                        String fullName = account == null 
		                        	? null 
		                        		: account.getFullName();
		                        if(fullName == null || fullName.startsWith(SecurityKeys.ADMIN_PRINCIPAL + SecurityKeys.ID_SEPARATOR)) {
		                            attendees.add(
		                                ";CN=" + emailAddress + (partyStatus == null ? "" : ";PARTSTAT=" + partyStatus) + ";ROLE=" + partyType + ";RSVP=TRUE:MAILTO:" + emailAddress
		                            );      
		                        } else {
		                            attendees.add(
		                                ";CN=\"" + fullName + " (" + emailAddress + ")\"" + (partyStatus == null ? "" : ";PARTSTAT=" + partyStatus) + ";ROLE=" + partyType + ";RSVP=TRUE:MAILTO:" + emailAddress
		                            );
		                        }
		                    }
                    	}
                    }
                } catch(ServiceException e) {
                    if(e.getExceptionCode() != BasicException.Code.AUTHORIZATION_FAILURE) {
                        throw e;
                    }
                }
            }
        }
        // ORGANIZER
        if(organizerEMail == null) {
        	if(activity.getAssignedTo() != null) {
	            organizerEMail = Accounts.getInstance().getPrimaryBusinessEMail(
	                activity.getAssignedTo(),
	                null
	            );
        	} else if(activity.getReportingContact() != null) {
                organizerEMail = Accounts.getInstance().getPrimaryBusinessEMail(
                	activity.getReportingContact(),
                    null
                );
        	}
        }
        // X_OPENCRX_RENDER_ALARMS
        boolean flagRenderAlarms = this.isFlagRenderAlarms(activity);
        // Return if data is missing
        if(!statusMessage.isEmpty()) {
            return null;
        }
        if(sourceIcal == null || sourceIcal.isEmpty()) {
            // Empty template
            UUID uid = null;
            try {
                uid = UUIDConversion.fromString(activity.refGetPath().getLastSegment().toClassicRepresentation());
            } catch(Exception e) {
                uid = UUIDs.newUUID();
            }
            sourceIcal =
                "BEGIN:VCALENDAR\n" +
                "PRODID:" + PROD_ID + "\n" +
                "VERSION:2.0\n" +
                (icalType == ICalendar.ICAL_TYPE_VTODO ? "BEGIN:VTODO\n" : "BEGIN:VEVENT\n") +
                "UID:" + UUIDConversion.toUID(uid) + "\n" +
                "LAST-MODIFIED:" + lastModified.substring(0, 15) + "Z\n" +
                "DTSTART:\n" +
                "DTEND:\n" +
                "DUE:\n" +
                "COMPLETED:\n" +
                "LOCATION:\n" +
                "DTSTAMP:\n" +
                "SUMMARY:\n" +
                "DESCRIPTION:\n" +
                "PRIORITY:\n" +
                "STATUS:\n" +
                "ATTENDEE:\n" +
                "CLASS:" + icalClass.name() + "\n" +
                (flagRenderAlarms ? X_OPENCRX_RENDER_ALARMS_TRUE : X_OPENCRX_RENDER_ALARMS_FALSE) + "\n" +
                (icalType == ICalendar.ICAL_TYPE_VTODO ? "END:VTODO\n" : "END:VEVENT\n") +
                "END:VCALENDAR";            
        }
        try {
            QuotaByteArrayOutputStream targetIcalBos = new QuotaByteArrayOutputStream(ICalendar.class.getName());
            PrintWriter targetIcal = new PrintWriter(new OutputStreamWriter(targetIcalBos, "UTF-8"));
            String lSourceIcal = null;
            BufferedReader readerSourceIcal = new BufferedReader(new StringReader(sourceIcal));
            boolean isEventOrTodo = false;
            boolean isAlarm = false;
            boolean isTimezone = false;
            String tagStart = null;
            int nEvents = 0;
            while((lSourceIcal = readerSourceIcal.readLine()) != null) {
                if(!lSourceIcal.startsWith(" ")) {
                    tagStart = lSourceIcal;
                }
                // PRODID
                if(lSourceIcal.startsWith("PRODID") || lSourceIcal.startsWith("prodid")) {                
                    targetIcal.println("PRODID:" + PROD_ID);
                } else if(lSourceIcal.startsWith("TZID") || lSourceIcal.startsWith("tzid")) {
                	// noop
                } else if(
                    lSourceIcal.toUpperCase().startsWith("BEGIN:VTIMEZONE") 
                ) {
                    isTimezone = true;
                } else if(
                    lSourceIcal.toUpperCase().startsWith("END:VTIMEZONE")
                ) {
                    isTimezone = false;
                } else if(
                    lSourceIcal.toUpperCase().startsWith("BEGIN:VALARM")
                ) {
                    targetIcal.println("BEGIN:VALARM");                    
                    isAlarm = true;
                } else if(
                    lSourceIcal.toUpperCase().startsWith("END:VALARM")
                ) {           
                    targetIcal.println("END:VALARM");                    
                    isAlarm = false;                    
                } else if(
                    lSourceIcal.toUpperCase().startsWith("BEGIN:VEVENT") ||
                    lSourceIcal.toUpperCase().startsWith("BEGIN:VTODO") 
                ) {
                    targetIcal.println(
                        icalType == ICAL_TYPE_VTODO ? "BEGIN:VTODO" : "BEGIN:VEVENT"
                    );
                    isEventOrTodo = true;
                }
                // Dump updated event fields only for first event
                else if(
                    (nEvents == 0) &&
                    lSourceIcal.toUpperCase().startsWith("END:VEVENT") || 
                    lSourceIcal.toUpperCase().startsWith("END:VTODO")
                ) {                    
                	boolean isAllDay =
                		((dtStart != null) && (dtStart.endsWith("T000000.000Z") || dtStart.endsWith("T000000Z"))) &&
                		((dtEnd != null) && (dtEnd.endsWith("T000000.000Z") || dtEnd.endsWith("T000000Z")));
                    // CLASS
                    if(icalClass != null) {
                    	targetIcal.println("CLASS:" + icalClass.name());
                    }
                	// DTSTART
                	if(dtStart != null) {
                		if(isAllDay) {
                			targetIcal.println("DTSTART;VALUE=DATE:" + dtStart.substring(0, 8));                            
                        } else {
                            targetIcal.println("DTSTART:" + dtStart.substring(0, 15) + "Z");
                        }
                	}
                	// DTEND
                	if(dtEnd != null) {
                		if(isAllDay) {
                			targetIcal.println("DTEND;VALUE=DATE:" + dtEnd.substring(0, 8));                            
                		} else {
                			targetIcal.println("DTEND:" + dtEnd.substring(0, 15) + "Z");
                		}
                	}
                    // DUE
                    if(dueBy != null) {
                        if(dueBy.endsWith("T000000.000Z")) {
                            targetIcal.println("DUE;VALUE=DATE:" + dueBy.substring(0, 8));                            
                        } else {
                            targetIcal.println("DUE:" + dueBy.substring(0, 15) + "Z");
                        }
                    }
                    // COMPLETED
                    if(completed != null) {
                        if(completed.endsWith("T000000.000Z")) {
                            targetIcal.println("COMPLETED;VALUE=DATE:" + completed.substring(0, 8));                            
                        } else {
                            targetIcal.println("COMPLETED:" + completed.substring(0, 15) + "Z");
                        }
                    }
                    // LAST-MODIFIED
                    if(lastModified != null) {
                        targetIcal.println("LAST-MODIFIED:" + lastModified.substring(0, 15) + "Z");
                    }
                    // LOCATION
                    if((location != null) && !location.isEmpty()) {
                        targetIcal.println("LOCATION:" + this.escapeControlChars(this.escapeCommas(location)));
                    }
                    // DTSTAMP
                    targetIcal.println("DTSTAMP:" + DateTimeFormat.BASIC_UTC_FORMAT.format(new Date()).substring(0, 15) + "Z");
                    // DESCRIPTION
                    if((description != null) && !description.isEmpty()) {
                        targetIcal.println("DESCRIPTION:" + this.escapeControlChars(description));
                    }
                    // SUMMARY
                    if((summary != null) && !summary.isEmpty()) {
                        targetIcal.println("SUMMARY:" + summary);
                    }
                    // PRIORITY
                    if(priority != null) {
                        int icalPriority = 9;
                        switch(priority.intValue()) {
                            case 1: icalPriority = 9; break; // low                            
                            case 2: icalPriority = 5; break; // normal
                            case 3: icalPriority = 3; break; // high
                            case 4: icalPriority = 2; break; // urgent
                            case 5: icalPriority = 1; break; // immediate
                        }
                        targetIcal.println("PRIORITY:" + icalPriority);
                    }
                    // STATUS
                    if(status != null) {
                        targetIcal.println("STATUS:" + status);
                    }
                    // ATTENDEE
                    for(int i = 0; i < attendees.size(); i++) {
                        targetIcal.println("ATTENDEE" + attendees.get(i));
                    }
                    // ORGANIZER
                    if(organizerEMail != null) {
                        targetIcal.println("ORGANIZER:MAILTO:" + organizerEMail);
                    }
                    // X_OPENCRX_HAS_ALARMS
                    targetIcal.println(flagRenderAlarms ? X_OPENCRX_RENDER_ALARMS_TRUE : X_OPENCRX_RENDER_ALARMS_FALSE);
                    if(
                        lSourceIcal.toUpperCase().startsWith("END:VEVENT") ||
                        lSourceIcal.toUpperCase().startsWith("END:VTODO")
                    ) {
                        targetIcal.println(
                            icalType == ICAL_TYPE_VTODO ? "END:VTODO" : "END:VEVENT"
                        );
                    }
                    isEventOrTodo = false;
                    nEvents++;
                } else if(isTimezone) {
                    // Skip all timezone fields. All datetime fields are converted to UTC
                } else if(
                    isEventOrTodo && 
                    !isAlarm && 
                    (nEvents == 0)
                ) {
                    boolean isUpdatableTag = 
                        tagStart.toUpperCase().startsWith("DTSTART");
                    isUpdatableTag |=
                        tagStart.toUpperCase().startsWith("DTEND");
                    isUpdatableTag |=
                        tagStart.toUpperCase().startsWith("DUE");
                    isUpdatableTag |=
                        tagStart.toUpperCase().startsWith("COMPLETED");
                    isUpdatableTag |=
                        tagStart.toUpperCase().startsWith("LOCATION");
                    isUpdatableTag |= 
                        tagStart.toUpperCase().startsWith("DTSTAMP");
                    isUpdatableTag |=
                        tagStart.toUpperCase().startsWith("DESCRIPTION");
                    isUpdatableTag |=
                        tagStart.toUpperCase().startsWith("LAST-MODIFIED");
                    isUpdatableTag |=
                        tagStart.toUpperCase().startsWith("SUMMARY");
                    isUpdatableTag |=
                        tagStart.toUpperCase().startsWith("PRIORITY");
                    isUpdatableTag |=
                        tagStart.toUpperCase().startsWith("STATUS");
                    isUpdatableTag |=
                        tagStart.toUpperCase().startsWith("ATTENDEE");
                    isUpdatableTag |=
                        tagStart.toUpperCase().startsWith("ORGANIZER");
                    isUpdatableTag |=
                        tagStart.toUpperCase().startsWith("CLASS");
                    isUpdatableTag |=
                    	tagStart.toUpperCase().startsWith(X_OPENCRX_RENDER_ALARMS);
                    if(!isUpdatableTag) {
                        targetIcal.println(lSourceIcal);
                    }
                } else {
                    targetIcal.println(lSourceIcal);                    
                }
            }
            targetIcal.flush();
            targetIcalBos.close();
            try {
                return targetIcalBos.toString("UTF-8");
            } catch(Exception e) {
                return null;
            }
        } catch(Exception e) {
            return null;
        }
    }

    /**
     * Remove proprietary attributes from ical, i.e. attributes having prefix X-.
     * 
     * @param ical
     */
    public void removeProprietaryProperties(
        Map<String,ICalField> ical
    ) {
        for(Iterator<String> i = ical.keySet().iterator(); i.hasNext(); ) {
            String prop = i.next();
            if(prop.startsWith("X-")) {
           		i.remove();
            }
        }
    }

    /**
     * Parse ical.
     * 
     * @param reader ical is read from reader.
     * @param icalAsString parsed ical is returned in StringBuilder in stringified form.
     * @return parsed ical.
     * @throws ServiceException
     */
    public Map<String,ICalField> parseICal(
        BufferedReader reader,
        StringBuilder icalAsString
    ) throws ServiceException {
        Map<String,ICalField> ical = new HashMap<String,ICalField>();
        List<String> lines = new ArrayList<String>();
        try {
	        String line = null;
	        while((line = reader.readLine()) != null) {
	        	String lineNoControls = new String();
	        	// Remove all non-whitespace control characters
	        	for(int i = 0; i < line.length(); i++) {
	        		char ch = line.charAt(i);
	        		lineNoControls += Character.isWhitespace(ch) || ch >= ' ' ? ch : '?'; 
	        	}
	        	lines.add(lineNoControls);
	        }
        } catch(Exception e) {}
        // Calendars with at most one event can be imported
        boolean isEvent = false;
        int nEvents = 0;
        try {
        	int l = 0;
        	parse: while(l < lines.size()) {
        		String line = lines.get(l++);
        		while(l < lines.size() && lines.get(l).startsWith(" ")) {
        			line += lines.get(l++).substring(1);
        		}
	            // Skip URLs AND ATTACHs if they reference an internal object.
	            if(
	                (line.startsWith("URL:") || line.startsWith("url:") || line.startsWith("URL;VALUE=URI:") || line.startsWith("ATTACH:") || line.startsWith("attach:")) &&
	                Base.getInstance().isAccessUrl(line.substring(line.indexOf(":") + 1))
	            ) {
	            	continue parse;
	            }
                icalAsString.append(line).append("\n");
                boolean addProperty = isEvent && (nEvents == 0);
                if(
                    line.startsWith("BEGIN:VEVENT") || 
                    line.startsWith("begin:vevent") || 
                    line.startsWith("BEGIN:VTODO") || 
                    line.startsWith("begin:vtodo") 
                ) {
                    isEvent = true;
                } else if(
                    line.startsWith("END:VEVENT") || 
                    line.startsWith("end:vevent") || 
                    line.startsWith("END:VTODO") || 
                    line.startsWith("end:vtodo") 
                ) {
                    nEvents++;
                    isEvent = false;
                    addProperty = false;
                }
                if(addProperty) {
                    int pos;
                	if((pos = line.indexOf(":")) >= 0) {
                    	String qualifiedFieldName = line.substring(0, pos).toUpperCase();
                        String[] fieldNameParts = qualifiedFieldName.split(";");
                        Map<String,List<String>> parameters = new HashMap<String,List<String>>();
                        for(int i = 1; i < fieldNameParts.length; i++) {
                        	String fieldNamePart = fieldNameParts[i];
                        	String parameterName = "TYPE";
                        	String[] parameterValues = new String[]{};
                        	if(fieldNamePart.indexOf("=") > 0) {
                        		int index = fieldNamePart.indexOf("=");
                        		parameterName = fieldNamePart.substring(0, index);
                        		parameterValues = fieldNamePart.substring(index + 1).split(",");
                        	} else {
                        		parameterName = "TYPE";
                        		parameterValues = fieldNamePart.split(",");
                        	}
                        	if(parameters.get(parameterName) != null) {
                        		parameters.get(parameterName).addAll(
                        			Arrays.asList(parameterValues)
                        		);
                        	} else {
                        		parameters.put(
                        			parameterName, 
                        			new ArrayList<String>(Arrays.asList(parameterValues))
                        		);
                        	}
                        }
                        String attributeValue = line.substring(pos + 1, line.length()).replace("\\n", "\n").replace("\\", "");
                        String attributeName = fieldNameParts[0];
                        {
	                        // Add parameter CN if missing for attribute ATTENDEE 
	                        if(
	                        	attributeName.equalsIgnoreCase("ATTENDEE") && 
	                        	parameters.get("CN") == null &&
	                        	attributeValue.startsWith("mailto:") || attributeValue.startsWith("MAILTO:")
	                        ) {
	                        	String cn = attributeValue.substring(7);
	                        	qualifiedFieldName += ";CN=" + cn;
	                        	parameters.put(
	                        		"CN",
	                        		Collections.singletonList(cn)
	                        	);
	                        }
                        }
                        // Check for duplicate attributes
                        {
	                        ICalField existingField = ical.get(
	                        	qualifiedFieldName
	                        );
	                        ICalField newField = new ICalField(
	                        	attributeName,
	                        	attributeValue,
	                        	parameters
	                        );
	                        if(ical.get(qualifiedFieldName) != null) {
	                        	SysLog.log(Level.WARNING, "ICAL has duplicate fields. Existing={0}, Ignored={1}, ICAL={2}", existingField, newField, lines);
	                        } else {                        
		                        ical.put(
		                            qualifiedFieldName,
		                            newField
		                        );
	                        }
                        }
                	}
	            }
	        }
        } catch(Exception e) {
        	throw new ServiceException(e);
        }
        this.removeProprietaryProperties(ical);
        return ical;
    }
    
    /**
     * Import ical item and map to activity.
     * 
     * @param item
     * @param activity
     * @param locale
     * @param errors
     * @param report
     * @return
     * @throws ServiceException
     */
    public BasicObject importItem(
        byte[] item,
        Activity activity,
        short locale,
        List<String> errors,
        List<String> report
    ) throws ServiceException {
    	PersistenceManager pm = JDOHelper.getPersistenceManager(activity);    	
    	org.opencrx.kernel.account1.jmi1.Segment accountSegment = Accounts.getInstance().getAccountSegment(
    		pm, 
    		activity.refGetPath().getSegment(2).toClassicRepresentation(), 
    		activity.refGetPath().getSegment(4).toClassicRepresentation()
    	);
        InputStream is = new ByteArrayInputStream(item);
        BufferedReader reader = null;
        try {
        	reader = new BufferedReader(new InputStreamReader(is, "UTF-8"));
        } catch(UnsupportedEncodingException e) {}
        StringBuilder ical = new StringBuilder();
        Map<String,ICalField> icalFields = this.parseICal(
            reader,
            ical
        );
        SysLog.trace("ICalendar", icalFields);
        return this.importItem(
            ical.toString(),
            icalFields,
            activity,
            accountSegment,
            locale,
            errors,
            report
        );
    }

    /**
     * Map attendee to contact.
     * 
     * @param attendeeAsString
     * @param accountSegment
     * @param existingContact
     * @param locale
     * @param report
     * @return
     * @throws ServiceException
     */
    protected Account getAttendeeAsContact(
        String attendeeAsString,
        org.opencrx.kernel.account1.jmi1.Segment accountSegment,
        Contact existingContact,
        short locale,
        List<String> report
    ) throws ServiceException {
        int pos = attendeeAsString.indexOf("MAILTO:");
        if(pos < 0) {
            pos = attendeeAsString.indexOf("mailto:");
        }
        String emailInternet = attendeeAsString.substring(pos + 7);
        PersistenceManager pm = JDOHelper.getPersistenceManager(accountSegment);
        String providerName = accountSegment.refGetPath().getSegment(2).toClassicRepresentation();
        String segmentName = accountSegment.refGetPath().getSegment(4).toClassicRepresentation();
        List<EMailAddress> emailAddresses = Accounts.getInstance().lookupEmailAddress(
        	pm,
        	providerName, 
        	segmentName, 
        	emailInternet
        );
        if(!emailAddresses.isEmpty()) {
        	if(existingContact != null) {
        		for(EMailAddress emailAddress: emailAddresses) {
        			if(emailAddress.refGetPath().startsWith(existingContact.refGetPath())) {
        				return existingContact;
        			}
        		}
        	}
        	EMailAddress emailAddress = emailAddresses.iterator().next();        	
        	return (Account)pm.getObjectById(emailAddress.refGetPath().getParent().getParent());
        } else {
        	return null;
        }
    }

    /**
     * Get dateTime as UTC timestamp.
     * 
     * @param dateTime
     * @param tz
     * @return
     * @throws ParseException
     */
    protected Date getUtcDate(
        String dateTime,
        TimeZone tz
    ) throws ParseException {
        Date date = null;
        if(dateTime.endsWith("Z")) {
            if(dateTime.length() == 16) {
                date = DateTimeFormat.BASIC_UTC_FORMAT.parse(dateTime.substring(0, 15) + ".000Z");
            }
            else {
                date = DateTimeFormat.BASIC_UTC_FORMAT.parse(dateTime);
            }
        } else if(dateTime.length() == 8) {
            date = DateTimeFormat.BASIC_UTC_FORMAT.parse(dateTime + "T000000.000Z");
        } else {
            SimpleDateFormat dateTimeFormatter = new SimpleDateFormat(DATETIME_FORMAT);
            dateTimeFormatter.setLenient(false);
            dateTimeFormatter.setTimeZone(tz);        	
            date = dateTimeFormatter.parse(dateTime);
        }
        return date;
    }

    /**
     * Unescape ical field.
     * 
     * @param s
     * @return
     */
    protected String unescapeField(
        String s
    ) {
        String t = s.replace("\\\\", "\\");
        t = t.replace("\\;", ";");
        t = t.replace("\\,", ",");
        t = t.replace("\\\"", "\"");
        return t;
    }

    /**
     * Update activity party type. Only modify if mapping is deterministic.
     * 
     * @param party
     * @param newPartyType
     */
    protected void updateActivityPartyType(
    	AbstractActivityParty party,
    	PartyType newPartyType
    ) {
    	switch(newPartyType) {
    		case REQUIRED: 
    		case OPTIONAL:
    		case ORGANIZER:
    			party.setPartyType(newPartyType.getValue());
    			break;
    		case NA:
    			if(
    				party.getPartyType() == PartyType.REQUIRED.getValue() ||
    				party.getPartyType() == PartyType.OPTIONAL.getValue() ||
    				party.getPartyType() == PartyType.ORGANIZER.getValue()
    			) {
    				party.setPartyType(PartyType.NA.getValue());
    			} {
    				// Do not change if existing party is an unknown PartyType 
    			}
    			break;
    		default:
    			break;
    	}
    }

    /**
     * Update activity party status. Only update if new status is not NA.
     * 
     * @param party
     * @param newPartyStatus
     */
    protected void updateActivityPartyStatus(
    	AbstractActivityParty party,
    	PartyStatus newPartyStatus
    ) {
    	if(newPartyStatus != PartyStatus.NA) {
    		party.setPartyStatus(newPartyStatus.getValue());
    	}
    }
    
    /**
     * Map party email address to EMailAddress. Return matching addresses.
     * For PartyType.ORGANIZER an EMailAddress is created on-the-fly
     * and assigned to Accounts.getUnassignableAddressesHolder() if it does 
     * not exist. Override this method for custom-specific behaviour.
     * 
     * @param pm
     * @param providerName
     * @param segmentName
     * @param email
     * @param partyType
     * @return
     * @throws ServiceException
     */
    protected List<EMailAddress> mapPartyEMail(
    	PersistenceManager pm,
    	String providerName,
    	String segmentName,
    	String email,
    	PartyType partyType,
    	PartyStatus partyStatus
    ) throws ServiceException {
    	if(PartyType.ORGANIZER.equals(partyType)) {
    		return Accounts.getInstance().lookupEmailAddress(
        		pm, 
        		providerName, 
        		segmentName, 
        		email,
        		false, // exactCaseInsensitiveOnly
        		true // forceCreate
        	);
    	} else {
    		return Accounts.getInstance().lookupEmailAddress(
        		pm, 
        		providerName, 
        		segmentName, 
        		email
        	);
    	}
    }

    /**
     * Map fields of ical to activity.
     * 
     * @param icalAsString
     * @param ical
     * @param activity
     * @param accountSegment
     * @param locale
     * @param errors
     * @param report
     * @return
     * @throws ServiceException
     */
    public Activity importItem(
        String icalAsString,
        Map<String,ICalField> ical,
        Activity activity,
        org.opencrx.kernel.account1.jmi1.Segment accountSegment,
        short locale,
        List<String> errors,
        List<String> report
    ) throws ServiceException {
    	PersistenceManager pm = JDOHelper.getPersistenceManager(accountSegment);
		String providerName = activity.refGetPath().getSegment(2).toClassicRepresentation();
		String segmentName = activity.refGetPath().getSegment(4).toClassicRepresentation();
        // ATTENDEEs
        List<EMailAddress> partyEmails = new ArrayList<EMailAddress>();
        List<PartyType> partyTypes = new ArrayList<PartyType>();
        List<PartyStatus> partyStatuses = new ArrayList<PartyStatus>();
        for(ICalField attendee: ICalField.findFields("ATTENDEE", null, null, ical)) {
            if(
                (attendee.getValue().indexOf("MAILTO:") >= 0) ||
                (attendee.getValue().indexOf("mailto:") >= 0)
            ) {
            	int pos = attendee.getValue().indexOf("MAILTO:");
            	if(pos < 0) {
            		pos = attendee.getValue().indexOf("mailto:");
            	}
            	if(pos >= 0) {
            		try {
		                InternetAddress email = new InternetAddress(attendee.getValue().substring(pos + 7), true);
		                PartyType partyType = PartyType.REQUIRED;
	            		if(attendee.getParameters().get("ROLE") != null && !attendee.getParameters().get("ROLE").isEmpty()) {
	            			String role = attendee.getParameters().get("ROLE").get(0);
	            			if("REQ-PARTICIPANT".equals(role)) {
	            				partyType = PartyType.REQUIRED;
	            			} else if("OPT-PARTICIPANT".equals(role)) {
	            				partyType = PartyType.OPTIONAL;
	            			} else {
		            			partyType = PartyType.NA;	            				
	            			}
	            		}
	            		PartyStatus partyStatus = PartyStatus.NA;
	                    if(attendee.getParameters().get("PARTSTAT") != null && !attendee.getParameters().get("PARTSTAT").isEmpty()) {
		                    if("NEEDS-ACTION".equals(attendee.getParameters().get("PARTSTAT").get(0))) {
		                    	partyStatus = PartyStatus.NEEDS_ACTION;
		                    } else if("ACCEPTED".equals(attendee.getParameters().get("PARTSTAT").get(0))) {
		                    	partyStatus = PartyStatus.ACCEPTED;
		                    } else if("DECLINED".equals(attendee.getParameters().get("PARTSTAT").get(0))) {
		                    	partyStatus = PartyStatus.DECLINED;
		                    } else if("TENTATIVE".equals(attendee.getParameters().get("PARTSTAT").get(0))) {
		                    	partyStatus = PartyStatus.TENTATIVE;
		                    } else if("DELEGATED".equals(attendee.getParameters().get("PARTSTAT").get(0))) {
		                    	partyStatus = PartyStatus.DELEGATED;
		                    } else if("COMPLETED".equals(attendee.getParameters().get("PARTSTAT").get(0))) {
		                    	partyStatus = PartyStatus.COMPLETED;
		                    } else {
		                    	partyStatuses.add(PartyStatus.NA);
		                    }
	                    }
		            	List<EMailAddress> emailAddresses = this.mapPartyEMail(
		            		pm,
		            		providerName, 
		            		segmentName, 
		            		email.getAddress(), 
		            		partyType,
		            		partyStatus
		            	);
		            	if(!emailAddresses.isEmpty()) {
		            		partyEmails.add(emailAddresses.iterator().next());
		            		partyTypes.add(partyType);
		            		partyStatuses.add(partyStatus);
		            	}
            		} catch(Exception e) {
            			ServiceException e0 = new ServiceException(
            				e,
            				BasicException.Code.DEFAULT_DOMAIN,
            				BasicException.Code.PARSE_FAILURE,
            				"Invalid InternetAddress. Ignoring Attendee.",
            				new BasicException.Parameter("address", attendee.getValue())
            			);
            			e0.log();
            		}
            	}
            }
        }
        if(!errors.isEmpty()) {
            return null;
        }
        // ORGANIZER 
        String s = ICalField.getFieldValue("ORGANIZER", ical);
        if((s != null) && !s.isEmpty()) {
        	int pos = s.indexOf("MAILTO:");
        	if(pos < 0) {
        		pos = s.indexOf("mailto:");
        	}
        	if(pos >= 0) {
                String email = s.substring(pos + 7);
            	List<EMailAddress> emailAddresses = Accounts.getInstance().lookupEmailAddress(
            		pm, 
            		providerName, 
            		segmentName, 
            		email,
            		false, // exactCaseInsensitiveOnly
            		true // forceCreate
            	);
            	if(!emailAddresses.isEmpty()) {
            		partyEmails.add(
            			emailAddresses.iterator().next()
            		);
            		partyTypes.add(PartyType.ORGANIZER);
            		partyStatuses.add(PartyStatus.ACCEPTED);
            	}
        	}
        }
        // externalLink
        boolean hasUid = false;
        boolean hasRecurrenceId = false;
        String uid = ical.get("UID") == null 
        	? activity.refGetPath().getLastSegment().toClassicRepresentation()
        	: ICalField.getFieldValue("UID", ical);
        String recurrenceId = ICalField.getFieldValue("RECURRENCE-ID", ical);
        List<String> externalLinks = new ArrayList<String>();        
        for(String externalLink: activity.getExternalLink()) {
            if(externalLink.startsWith(ICAL_SCHEMA)) {
                externalLinks.add(ICAL_SCHEMA + uid);
                hasUid = true;
            } else if(externalLink.startsWith(ICAL_RECURRENCE_ID_SCHEMA)) {
            	if(recurrenceId != null && !recurrenceId.isEmpty()) {
	                externalLinks.add(ICAL_RECURRENCE_ID_SCHEMA + recurrenceId);
	                hasRecurrenceId = true;
            	}
            } else {
            	externalLinks.add(externalLink);
            }
        }
        if(!hasUid) {
            externalLinks.add(ICAL_SCHEMA + uid);
        }
        if(!hasRecurrenceId && recurrenceId != null && !recurrenceId.isEmpty()) {
            externalLinks.add(ICAL_RECURRENCE_ID_SCHEMA + recurrenceId);
        }
        activity.getExternalLink().clear();
        activity.getExternalLink().addAll(externalLinks);
        // DTSTART
        String dtStart = ICalField.getFieldValue("DTSTART", ical);
        if((dtStart != null) && !dtStart.isEmpty()) {
            try {
                activity.setScheduledStart(
                	this.getUtcDate(
                		dtStart,
                        ICalField.getField("DTSTART", null, null, ical).getTimeZone()
                    )
                );
            } catch(Exception e) {
                errors.add("DTSTART (" + dtStart + ")");
            }
        }
        // DTEND
        String dtEnd = ICalField.getFieldValue("DTEND", ical);
        if((dtEnd != null) && !dtEnd.isEmpty()) {
            try {
                activity.setScheduledEnd(
                	this.getUtcDate(
                        dtEnd, 
                        ICalField.getField("DTEND", null, null, ical).getTimeZone()
                    )
                );
                // If DTSTART and DTEND are specified as DATETIME prevent that the
                // event is converted to an all-day event. Allow all-day events
                // only if DTSTART and DTEND are specified in as DATE.
                if(
                	activity.getScheduledStart() != null &&
                	(activity.getScheduledEnd().getTime() - activity.getScheduledStart().getTime() == 86400000L) &&
                	(dtStart.length() > 8 || dtEnd.length() > 8)
                ) {
                    activity.setScheduledEnd(
                    	new Date(activity.getScheduledEnd().getTime() - 1000L)
                    );                	
                }
            } catch(Exception e) {
                errors.add("DTEND (" + dtEnd + ")");
            }
        }
        // DUE
        s = ICalField.getFieldValue("DUE", ical);
        if((s != null) && !s.isEmpty()) {
            try {
                activity.setDueBy(
                	this.getUtcDate(
                        s, 
                        ICalField.getField("DUE", null, null, ical).getTimeZone()
                    )
                );
            } catch(Exception e) {
                errors.add("DUE (" + s + ")");
            }
        }
        // COMPLETED
        s = ICalField.getFieldValue("COMPLETED", ical);
        if((s != null) && !s.isEmpty()) {
            try {
                activity.setActualEnd(
                	this.getUtcDate(
                        s, 
                        ICalField.getField("COMPLETED", null, null, ical).getTimeZone()
                    )
                );
            } catch(Exception e) {
                errors.add("COMPLETED (" + s + ")");
            }
        }
        else {
            activity.setActualEnd(null);            
        }
        // PRIORITY
        s = ICalField.getFieldValue("PRIORITY", ical);
        if((s != null) && !s.isEmpty()) {
            try {
                int priority = 1;
                switch(Integer.valueOf(s).intValue()) {
                    case 1: priority = 5; break; // immediate
                    case 2: priority = 4; break; // urgent
                    case 3:
                    case 4: priority = 3; break; // high
                    case 5: 
                    case 6:
                    case 7:
                    case 8: priority = 2; break; // normal
                    case 9: priority = 1; break; // low
                }
                activity.setPriority(new Short((short)priority));
            } catch(Exception e) {
                errors.add("PRIORITY (" + s + ")");
            }
        }
        // SUMMARY
        s = ICalField.getFieldValue("SUMMARY", ical);
        if((s != null) && !s.isEmpty()) {
        	// Parse SUMMARY and map it to activity:
        	// name [ "//" comment [ "[" misc1 "]" ] ]
            int posComment = s.startsWith(Base.COMMENT_SEPARATOR_BOT) ? 0 : s.lastIndexOf(Base.COMMENT_SEPARATOR_EOT);
            String name = null;
            String misc1 = null;
            if(posComment >= 0) {
            	String comment = s.substring(posComment + Base.COMMENT_SEPARATOR_EOT.length());
            	name = s.substring(0, posComment).trim();
            	int posMiscSuffix = comment.indexOf("[");
            	if(posMiscSuffix > 0 && comment.endsWith("]")) {
            		misc1 = comment.substring(posMiscSuffix);
            	}
            } else {
            	name = s;
            }
            name = this.unescapeField(name);
            // Limit name to 1000 chars
            if(name.length() > 1000) {
                name = name.substring(0, 1000);
            }
            activity.setName(name);
            // Do not reset misc1 if it does not match pattern [...]
            if(
            	activity.getMisc1() == null || 
            	(activity.getMisc1().startsWith("[") && activity.getMisc1().endsWith("]"))
            ) {
            	activity.setMisc1(misc1 == null ? null : misc1);
            }
        }
        // DESCRIPTION
        s = ICalField.getFieldValue("DESCRIPTION", ical);
        if((s != null) && !s.isEmpty()) {
            int posComment = s.startsWith(Base.COMMENT_SEPARATOR_BOT) ? 0 : s.lastIndexOf(Base.COMMENT_SEPARATOR_EOT);
            s =  posComment >= 0 ? 
                s.substring(0, posComment) : 
                s;        	
            String temp = "";
            int pos = 0;
            while((pos = s.indexOf("\\n")) >= 0) {
                temp += temp.length() == 0 ? "" : "\n";
                temp += s.substring(0, pos);
                s = s.substring(pos + 2);
            }
            temp += temp.length() == 0 ? "" : "\n";
            temp += s;
            // Limit description to 1000 chars
            String description = this.unescapeField(temp);
            if(description.length() > 1000) {
                description = description.substring(0, 1000);
            }
            activity.setDescription(description);
        }
        // LOCATION
        s = ICalField.getFieldValue("LOCATION", ical);
        if((s != null) && !s.isEmpty()) {
            activity.setLocation(
            	this.unescapeField(s)
            );
        }
        // CLASS
        s = ICalField.getFieldValue("CLASS", ical);
        if((s != null) && !s.isEmpty()) {
        	try {
	            activity.setIcalClass(
	            	ICalClass.valueOf(s.toUpperCase()).getValue()
	            );
        	} catch(Exception e) {}
        }
        // ical
        activity.setIcal(icalAsString);
        if(!errors.isEmpty()) {
            return null;
        }        
        report.add("Update activity");
        // Add attendees
        List<Account> accountParties = new ArrayList<Account>();
        nextParty: for(int i = 0; i < partyEmails.size(); i++) {
        	EMailAddress partyEmail = partyEmails.get(i);
            if(activity instanceof EMail) {
            	EMailRecipientQuery query = (EMailRecipientQuery)pm.newQuery(EMailRecipient.class);
            	query.thereExistsParty().equalTo(partyEmail);
            	List<EMailRecipient> emailRecipients = ((EMail)activity).getEmailRecipient(query);
            	EMailRecipient emailRecipient = emailRecipients.isEmpty() ?
            		null :
            			emailRecipients.iterator().next();
            	if(emailRecipient == null) {
                	emailRecipient = pm.newInstance(EMailRecipient.class);
                    ((EMail)activity).addEmailRecipient(
                    	this.getUidAsString(),
                    	emailRecipient
                    );
            	}
            	emailRecipient.setParty(partyEmail);
            	switch(partyTypes.get(i)) {
            		case REQUIRED:
            			emailRecipient.setPartyType(PartyType.EMAIL_TO.getValue());
            			break;
            		case OPTIONAL:
                		emailRecipient.setPartyType(PartyType.EMAIL_CC.getValue());
                		break;
            		case ORGANIZER:
            			emailRecipient.setPartyType(PartyType.EMAIL_FROM.getValue());
            			break;
                	default:
                    	emailRecipient.setPartyType(partyTypes.get(i).getValue());
                    	break;
            	}
            	this.updateActivityPartyStatus(
            		emailRecipient, 
            		partyStatuses.get(i)
            	);
            	emailRecipient.setEmailHint(partyEmail.getEmailAddress());
            } else if(activity instanceof Incident) {
            	Account partyHolder = (Account)pm.getObjectById(partyEmail.refGetPath().getParent().getParent());
            	IncidentPartyQuery query = (IncidentPartyQuery)pm.newQuery(IncidentParty.class);
            	// At most one participant with role ORGANIZER
            	if(partyTypes.get(i) == PartyType.ORGANIZER) {
            		query.partyType().equalTo(PartyType.ORGANIZER.getValue()); 
            	} else {
            		query.thereExistsParty().equalTo(partyHolder);
	            	// Need hint if account is duplicate
	            	if(accountParties.contains(partyHolder)) {
	            		query.thereExistsEmailHint().equalTo(partyEmail.getEmailAddress());
	            	}
            	}
            	accountParties.add(partyHolder);
            	List<IncidentParty> incidentParties = ((Incident)activity).getIncidentParty(query);
            	IncidentParty incidentParty = incidentParties.isEmpty() ? null : incidentParties.iterator().next();
            	if(incidentParty == null) {
            		incidentParty = pm.newInstance(IncidentParty.class);
	                ((Incident)activity).addIncidentParty(
	                	this.getUidAsString(),
	                	incidentParty
	                );            		
            	}
            	incidentParty.setParty(partyHolder);
            	this.updateActivityPartyType(
            		incidentParty, 
            		partyTypes.get(i)
            	);
            	this.updateActivityPartyStatus(
            		incidentParty, 
            		partyStatuses.get(i)
            	);
                incidentParty.setEmailHint(partyEmail.getEmailAddress());
            } else if(activity instanceof Mailing) {
            	// Can not map to party. Mailings need postal addresses 
            	// whereas ICal attendees are E-mail addresses
            } else if(activity instanceof Meeting) {
            	Account partyHolder = (Account)pm.getObjectById(partyEmail.refGetPath().getParent().getParent());
            	MeetingPartyQuery query = (MeetingPartyQuery)pm.newQuery(MeetingParty.class);
            	if(partyTypes.get(i) == PartyType.ORGANIZER) {
            		query.partyType().equalTo(PartyType.ORGANIZER.getValue());
            	} else {
	            	query.thereExistsParty().equalTo(partyHolder);
	            	if(accountParties.contains(partyHolder)) {
	            		query.thereExistsEmailHint().equalTo(partyEmail.getEmailAddress());
	            	}
            	}
            	List<MeetingParty> meetingParties = ((Meeting)activity).getMeetingParty(query);
            	MeetingParty meetingParty = meetingParties.isEmpty() ? null : meetingParties.iterator().next();
            	if(meetingParty == null) {
            		meetingParty = pm.newInstance(MeetingParty.class);
	                ((Meeting)activity).addMeetingParty(
	                	this.getUidAsString(),
	                	meetingParty
	                );            		
            	} else if(partyTypes.get(i) == PartyType.ORGANIZER) {
            		// Do not change organizer if it already exists
            		accountParties.add(meetingParty.getParty());
            		continue nextParty;
            	}
        		accountParties.add(partyHolder);
            	meetingParty.setParty(partyHolder);
            	this.updateActivityPartyType(
            		meetingParty, 
            		partyTypes.get(i)
            	);
            	this.updateActivityPartyStatus(
            		meetingParty, 
            		partyStatuses.get(i)
            	);
                meetingParty.setEmailHint(
                	partyEmail.getEmailAddress()
                );
            } else if(activity instanceof PhoneCall) {
            	// Can not map to party. PhoneCalls need phone numbers 
            	// whereas ICal attendees are E-mail addresses
            } else if(activity instanceof Task) {
            	Account partyHolder = (Account)pm.getObjectById(partyEmail.refGetPath().getParent().getParent());
            	TaskPartyQuery query = (TaskPartyQuery)pm.newQuery(TaskParty.class);
            	if(partyTypes.get(i) == PartyType.ORGANIZER) {
            		query.partyType().equalTo(PartyType.ORGANIZER.getValue());
            	} else {
	            	query.thereExistsParty().equalTo(partyHolder);
	            	if(accountParties.contains(partyHolder)) {
	            		query.thereExistsEmailHint().equalTo(partyEmail.getEmailAddress());
	            	}
            	}
        		accountParties.add(partyHolder);
            	List<TaskParty> taskParties = ((Task)activity).getTaskParty(query);
            	TaskParty taskParty = taskParties.isEmpty() ? null : taskParties.iterator().next();
            	if(taskParty == null) {
            		taskParty = pm.newInstance(TaskParty.class);
	                ((Task)activity).addTaskParty(
	                	this.getUidAsString(),
	                	taskParty
	                );
            	}
            	taskParty.setParty(partyHolder);
            	this.updateActivityPartyType(
            		taskParty, 
            		partyTypes.get(i)
            	);
            	this.updateActivityPartyStatus(
            		taskParty, 
            		partyStatuses.get(i)
            	);
                taskParty.setEmailHint(partyEmail.getEmailAddress());
            }
        }
        return activity;
    }

    /**
     * Find activity matching the given criteria.
     * 
     * @param activitiesHelper
     * @param icalUid
     * @param icalRecurrenceId
     * @return
     */
    protected Activity findActivity(
        ActivityQueryHelper activitiesHelper,
        String icalUid,
        String icalRecurrenceId 
    ) {
    	PersistenceManager pm = activitiesHelper.getPersistenceManager();
        ActivityQuery query = (ActivityQuery)pm.newQuery(Activity.class);
        query.thereExistsExternalLink().equalTo(ICalendar.ICAL_SCHEMA + icalUid);
        if(icalRecurrenceId == null) {
        	query.forAllExternalLink().startsNotWith(ICalendar.ICAL_RECURRENCE_ID_SCHEMA);
        } else {
            query.thereExistsExternalLink().equalTo(ICalendar.ICAL_RECURRENCE_ID_SCHEMA + icalRecurrenceId);       
        }
        Collection<Activity> activities = activitiesHelper.getFilteredActivities(query);
        if(activities.isEmpty()) {
            query = (ActivityQuery)pm.newQuery(Activity.class);
            query.thereExistsExternalLink().equalTo(ICalendar.ICAL_SCHEMA + icalUid.replace('.', '+'));
            if(icalRecurrenceId == null) {
            	query.forAllExternalLink().startsNotWith(ICalendar.ICAL_RECURRENCE_ID_SCHEMA);
            }            
            else {
            	query.thereExistsExternalLink().equalTo(ICalendar.ICAL_RECURRENCE_ID_SCHEMA + icalRecurrenceId);    
            }
            activities = activitiesHelper.getFilteredActivities(query);
            if(activities.isEmpty()) {
                return null;
            } else {
                if(activities.size() > 1) {
                	SysLog.warning("Duplicate activities. Will be handled as not found", activities);
                    return null;
                } else {
                    return activities.iterator().next();
                }
            }
        } else {
            if(activities.size() > 1) {
            	SysLog.warning("Duplicate activities. Will not update", activities.iterator().next().refMofId());
                return null;
            } else {
                return activities.iterator().next();
            }
        }
    }

    /**
     * PutICalResult
     *
     */
    public static class PutICalResult {
    	
    	public enum Status {
    		CREATED, 
    		UPDATED, 
    		ERROR
    	}
    	
    	public PutICalResult(
    		Status status,
    		Activity activity
    	) {
    		this.status = status;
    		this.activity = activity;
    	}
    	public Status getStatus() {
        	return status;
        }
		public Activity getActivity() {
        	return activity;
        }
		private final Status status;
    	private final Activity activity;    	
    }
    
    /**
     * Map ICAL to activity. Try to lookup matching activity according to 
     * ICALs UID. If activity is not found create a new one if allowCreation is true.
     * 
     * @param reader
     * @param activitiesHelper
     * @param allowCreation
     * @param resourceId
     * @return
     * @throws ServiceException
     */
    public PutICalResult putICal(
    	BufferedReader reader,
    	ActivityQueryHelper activitiesHelper,
    	boolean allowCreation,
    	String resourceId
    ) throws ServiceException { 
    	PersistenceManager pm = activitiesHelper.getPersistenceManager();
    	PutICalResult.Status status = PutICalResult.Status.UPDATED;
    	Activity activity = null;
    	String calUID = null;
    	String l;
        try {
	        while((l = reader.readLine()) != null) {
	            boolean isEvent = l.startsWith("BEGIN:VEVENT");
	            boolean isTodo = l.startsWith("BEGIN:VTODO");
	            if(isEvent || isTodo) {
	                String calendar = "";
	                List<StringBuilder> alarms = new ArrayList<StringBuilder>();
	                calendar += "BEGIN:VCALENDAR\n";
	                calendar += "VERSION:2.0\n";
	                calendar += "PRODID:-" + ICalendar.PROD_ID + "\n";
	                calendar += isEvent ? "BEGIN:VEVENT\n" : "BEGIN:VTODO\n";
	                String recurrenceId = null;
	                boolean hasClass = false;
	                StringBuilder alarm = null;
	                while((l = reader.readLine()) != null) {
	                    if(l.startsWith("BEGIN:VALARM")) {
	                    	alarms.add(alarm = new StringBuilder());
	                    }
	                    if(alarm != null) {
	                    	alarm.append(l);
	                    	alarm.append("\n");
	                    } else {
                			calendar += l;
                			calendar += "\n";
	                	}
	                    if(l.startsWith("UID:")) {
	                        calUID = l.substring(4);
	                    } else if(l.startsWith("CLASS:")) {
	                        hasClass = true;
	                    } else if(l.startsWith("RECURRENCE-ID:")) {
	                        recurrenceId = l.substring(14);
	                    } else if(l.startsWith("END:VALARM")) {
	                    	alarm = null;
	                    } else if(l.startsWith("END:VEVENT") || l.startsWith("END:VTODO")) {
	                        break;
	                    }
	                }
	                calendar += "END:VCALENDAR\n";
	                SysLog.detail("Calendar", calendar);
	                SysLog.detail("Alarms", alarms);
	                if(resourceId != null) {
	                	BasicObject source = activitiesHelper.getSource();
	                	activity = activitiesHelper.getActivitySegment().getActivity(resourceId);
	                	if(activity != null) {
	                		ActivityQuery activityQuery = (ActivityQuery)pm.newQuery(Activity.class);
	                		activityQuery.thereExistsActivityNumber().equalTo(activity.getActivityNumber());
		                	if(source instanceof ActivityGroup) {
		                		ActivityGroup activityGroup = (ActivityGroup)source;
		                		activity = activityGroup.getFilteredActivity(activityQuery).isEmpty() ? null : activity;
		                	} else if(source instanceof AbstractFilterActivity) {
		                		AbstractFilterActivity activityFilter = (AbstractFilterActivity)source;
		                		activity = activityFilter.getFilteredActivity(activityQuery).isEmpty() ? null : activity;
		                	} else if(source instanceof UserHome) {
		                		UserHome userHome = (UserHome)source;
		                		activity = userHome.getAssignedActivity(activityQuery).isEmpty() ? null : activity;
		                	} else if(source instanceof Resource) {
		                		Resource resource = (Resource)source;
		                		activity = resource.getAssignedActivity(activityQuery).isEmpty() ? null : activity;
		                	}
	                	}
	                	{
		                    StringBuilder dummy = new StringBuilder();
		                    Map<String,ICalField> newICal = new HashMap<String,ICalField>();
	                    	newICal = this.parseICal(
	                            new BufferedReader(new StringReader(calendar.toString())),
	                            dummy
	                        );
		                    newICal.remove("LAST-MODIFIED");
		                    newICal.remove("DTSTAMP");                               
		                    newICal.remove("CREATED");                               
		                    dummy.setLength(0);
		                    Map<String,ICalField> oldICal = null;
		                    if(activity == null) {
		                    	boolean rewriteUID = true;
		                    	if(recurrenceId != null && !recurrenceId.isEmpty()) {
		                    		// Try to find activity with supplied UID. If not found
		                    		// rewrite the supplied UID. This way the recurrence event
		                    		// can be correlated with the series event
				                    rewriteUID = this.findActivity(
				                        activitiesHelper, 
				                        calUID,
				                        null // recurrenceId
				                    ) == null;
		                    	}
		                    	if(rewriteUID) {
			                    	int pos1 = calendar.indexOf("UID:");
			                    	if(pos1 > 0) {
			                    		int pos2 = calendar.indexOf("\n", pos1);
			                    		if(pos2 > pos1) {
			                    			String newUID = Base.getInstance().getUidAsString();
			                    			calendar =
			                    				calendar.substring(0, pos1) + 
			                    				"UID:" + newUID + "\n" +
			                    				calendar.substring(pos2 + 1);		                    		
			                    		}
			                    	}
		                    	}
		                    } else {
		                    	try {
		                            oldICal = ICalendar.getInstance().parseICal(
		                                new BufferedReader(new StringReader(activity.getIcal())),
		                                dummy
		                            );
		                    	} catch(Exception ignore) {}
		                        oldICal.remove("LAST-MODIFIED");
		                        oldICal.remove("DTSTAMP");                                   
		                        oldICal.remove("CREATED");                                   
		                        oldICal.keySet().retainAll(newICal.keySet());
		                    }
		                    ActivityGroup activityGroup = activitiesHelper.getActivityGroup();
		                    boolean disabledIsModified = activity == null 
		                    	? false 
		                    	: !Utils.areEqual(
		                    		activity.isDisabled(), 
		                    		Boolean.valueOf(activitiesHelper.isDisabledFilter()
		                    	)
                            );
		                    // Update existing activity
		                    if(activity != null) {
		                    	this.updateTimers(
		                    		activity,
		                    		calendar,
		                    		alarms
		                    	);
		                    	if(!newICal.equals(oldICal) || disabledIsModified) {
			                        try {
			                        	if(!newICal.equals(oldICal)) {
			                        		boolean isTxLocal = !pm.currentTransaction().isActive();
			                        		if(isTxLocal) {
			                        			pm.currentTransaction().begin();
			                        		}
			            	                // Set CLASS if not supplied
			            	                if(!hasClass) {
			            	                	if(isEvent) {
			            	                		calendar = calendar.replace("BEGIN:VEVENT", "BEGIN:VEVENT\nCLASS:" + ICalClass.CONFIDENTIAL.name());
			            	                	} else {
			            	                		calendar = calendar.replace("BEGIN:VTODO", "BEGIN:VTODO\nCLASS:" + ICalClass.CONFIDENTIAL.name());	                		
			            	                	}
			            	                }
				                            ImportParams importItemParams = Structures.create(
				                            	ImportParams.class, 
				                            	Datatypes.member(ImportParams.Member.item, calendar.toString().getBytes("UTF-8")),
				                            	Datatypes.member(ImportParams.Member.itemMimeType, ICalendar.MIME_TYPE),
				                            	Datatypes.member(ImportParams.Member.itemName, "import.ics"),
				                            	Datatypes.member(ImportParams.Member.locale, (short)0)
				                            	
				                            );
				                            activity.importItem(importItemParams);
				                            if(isTxLocal) {
			                            		pm.currentTransaction().commit();
					                            pm.refresh(activity);
				                            }
			                        	}
			                        	if(disabledIsModified) {
				                            pm.currentTransaction().begin();
				                            // Handle as creation if activity is moved from folder non-disabled to disabled or vice-versa
				                            status = PutICalResult.Status.CREATED;
				                            activity.setDisabled(
				                                Boolean.valueOf(activitiesHelper.isDisabledFilter())
				                            );
				                            pm.currentTransaction().commit();
			                        	}
			                        } catch(Exception e) {
			                        	new ServiceException(e).log();
			                            try {
			                                pm.currentTransaction().rollback();
			                            } catch(Exception e0) {}
			                            status = PutICalResult.Status.ERROR;
			                        }
		                    	}
		                    }
		                    // Create new activity
		                    else if(
                                allowCreation && 		                    	
		                        (activity == null) &&
		                        (activityGroup != null)
		                    ) {
		                        Collection<ActivityCreator> activityCreators = activityGroup.getActivityCreator();
		                        // Priority 1
		                        ActivityCreator activityCreator = Activities.getInstance().findActivityCreator(
		                            activityCreators,
		                            isTodo 
		                            	? ActivityClass.TASK.getValue() 
		                            	: isEvent 
		                            		? ActivityClass.MEETING.getValue() 
		                            		: ActivityClass.INCIDENT.getValue()
		                        );
		                        // Priority 2
		                        if(activityCreator == null) {
		                            activityCreator = Activities.getInstance().findActivityCreator(
		                                activityCreators,
		                                isTodo 
		                                	? ActivityClass.MEETING.getValue() 
		                                	: isEvent 
		                                		? ActivityClass.INCIDENT.getValue() 
		                                		: ActivityClass.INCIDENT.getValue()
		                            );
		                        }
		                        // Priority 3
		                        if(activityCreator == null) {
		                            activityCreator = Activities.getInstance().findActivityCreator(
		                                activityCreators,
		                                isTodo 
		                                	? ActivityClass.INCIDENT.getValue() 
		                                	: isEvent 
		                                		? ActivityClass.INCIDENT.getValue() 
		                                		: ActivityClass.INCIDENT.getValue()
		                            );                
		                        }
		                        if(activityCreator == null) {
		                            activityCreator = activitiesHelper.getActivityGroup().getDefaultCreator();
		                        }
		                        if(activityCreator != null) {
		                            try {
		                                String name = "NA";
		                                int posSummary;
		                                if((posSummary = calendar.indexOf("SUMMARY:")) > 0) {
		                                    if(calendar.indexOf("\n", posSummary) > 0) {
		                                        name = calendar.substring(posSummary + 8, calendar.indexOf("\n", posSummary));
		                                    }
		                                }
		                                pm.currentTransaction().begin();
		                                NewActivityParams newActivityParams = Datatypes.create(
		                                	NewActivityParams.class,
		                                	Datatypes.member(
		                                		NewActivityParams.Member.icalType, 
		                                		isEvent 
		                                			? ICalendar.ICAL_TYPE_VEVENT 
		                                			: isTodo 
		                                				? ICalendar.ICAL_TYPE_VTODO 
		                                				: ICalendar.ICAL_TYPE_NA
		                                	),
		                                	Datatypes.member(
		                                		NewActivityParams.Member.name,
		                                		name
		                                	),
		                                	Datatypes.member(
		                                		NewActivityParams.Member.priority,
		                                		(short)0
		                                	)
		                                );
		                                NewActivityResult result = activityCreator.newActivity(newActivityParams);
		                                pm.currentTransaction().commit();
		                                try {
		                                    activity = (Activity)pm.getObjectById(result.getActivity().refGetPath());
		                                    pm.currentTransaction().begin();
		                                    ImportParams importItemParams = Datatypes.create(
		                                    	ImportParams.class,
		                                    	Datatypes.member(ImportParams.Member.item, calendar.toString().getBytes("UTF-8")),
		                                    	Datatypes.member(ImportParams.Member.itemMimeType, ICalendar.MIME_TYPE),
		                                    	Datatypes.member(ImportParams.Member.itemName, "import.ics"),
		                                    	Datatypes.member(ImportParams.Member.locale, (short)0)
		                                    );
		                                    activity.importItem(importItemParams);
		                                    pm.currentTransaction().commit();
		                                    this.updateTimers(
		                                    	activity,
		                                    	calendar,
		                                    	alarms
		                                    );
		                                    pm.refresh(activity);
		                                    if(
		                                        activitiesHelper.isDisabledFilter() &&
		                                        ((activity.isDisabled() == null) || !activity.isDisabled().booleanValue())
		                                    ) {
		                                        pm.currentTransaction().begin();
		                                        activity.setDisabled(Boolean.TRUE);
		                                        pm.currentTransaction().commit();
		                                    }
		                                    status = PutICalResult.Status.CREATED;
		                                } catch(Exception e) {
		                                	SysLog.warning("Error importing calendar. Reason is", new String[]{calendar.toString(), e.getMessage()});
		                                    new ServiceException(e).log();
		                                    try {
		                                        pm.currentTransaction().rollback();
		                                    } catch(Exception e0) {}
		                                }
		                            } catch(Exception e) {
		                            	SysLog.warning("Can not create activity. Reason is", e.getMessage());
		                                new ServiceException(e).log();
		                                try {
		                                    pm.currentTransaction().rollback();
		                                } catch(Exception e0) {}
		                            }
		                        } else {
		                        	SysLog.detail("Skipping calendar. No activity creator found", calendar); 
		                        }
		                    } else {
		                    	SysLog.detail(
		                            "Skipping ", 
		                            new String[]{
		                                "UID: " + calUID, 
		                                "Activity.number: " + (activity == null ? null : activity.refMofId()),
		                                "Activity.modifiedAt:" + (activity == null ? null : activity.getModifiedAt())
		                            }
		                        );
		                    }
                        }
	                }
	            }    	
	        }
        } catch (IOException e) {
        	throw new ServiceException(e);
        }
        return new PutICalResult(
        	status,
        	activity
        );
    }

	/**
	 * AlarmAction
	 *
	 */
	public enum AlarmAction {
		
    	AUDIO((short)10),
		DISPLAY((short)20),
		EMAIL((short)30),
		PROCEDURE((short)40);
		
		private short value;
		
		private AlarmAction(
			short value
		) {
			this.value = value;
		}
		
		public short getValue(
		) {
			return this.value;
		}
		
		static public AlarmAction valueOf(
			short value
		) {
			switch(value) {
				case 10: return AUDIO;
				case 20: return DISPLAY;
				case 30: return EMAIL;
				case 40: return PROCEDURE;
				default: return DISPLAY;
			}
		}
		
	}

	/**
	 * ICalClass
	 *
	 */
	public enum ICalClass {
		
		NA((short)0),
		PRIVATE((short)1),
		CONFIDENTIAL((short)2),
		PUBLIC((short)3);
		
		private short value;
		
		private ICalClass(
			short value
		) {
			this.value = value;
		}
		
		public short getValue(
		) {
			return this.value;
		}
		
		public static ICalClass valueOf(
			short value
		) {
			switch(value) {
				case 0: return NA;
				case 1: return PRIVATE;
				case 2: return CONFIDENTIAL;
				case 3: return PUBLIC;
				default: return NA;
			}
		}
		
	}

	/**
	 * Print alarm tags for the given event.
	 * 
	 * @param p
	 * @param event
	 */
	public void printAlarms(
		PrintWriter p,
		Activity event
	) throws ServiceException {
		if(event.getIcal().indexOf(X_OPENCRX_RENDER_ALARMS_TRUE) > 0) {
			PersistenceManager pm = JDOHelper.getPersistenceManager(event);
			TimerQuery timerQuery = (TimerQuery)pm.newQuery(Timer.class);
			timerQuery.forAllDisabled().isFalse();
	        Collection<Timer> timers = event.getAssignedTimer(timerQuery);
	        UserHome userHome = UserHomes.getInstance().getUserHome(event.refGetPath(), pm);
	        for(Timer timer: timers) {
	        	if(timer.refGetPath().get(6).equals(userHome.refGetPath().getBase())) {
	        		p.println("BEGIN:VALARM");
	        		p.println("ACTION:DISPLAY");
	        		long triggerMinutes = (timer.getTimerStartAt().getTime() - event.getScheduledStart().getTime()) / 60000L;
	        		p.println("TRIGGER;VALUE=DURATION:" + (triggerMinutes < 0 ? "-" : "") + "PT" + Math.abs(triggerMinutes) + "M");
	        		p.println("REPEAT:" + (timer.getTriggerRepeat() == null ? 1 : timer.getTriggerRepeat()));
	        		p.println("DURATION:PT" + (timer.getTriggerIntervalMinutes() == null ? 15 : timer.getTriggerIntervalMinutes()) + "M");
	        		p.println("SUMMARY:" + timer.getName());
	        		if(timer.getDescription() != null) {
	        			p.println("DESCRIPTION:" + timer.getDescription());
	        		}
	        		p.println("END:VALARM");
	        	}
	        }
		}
	}

    /**
     * Map alarms to timers.
     * 
     * @param event
     * @param calendar
     * @param alarms
     * @throws ServiceException
     */
    public void updateTimers(
    	Activity event,
    	String calendar,
    	List<StringBuilder> alarms
    ) throws ServiceException {
    	PersistenceManager pm = JDOHelper.getPersistenceManager(event);
        UserHome userHome = UserHomes.getInstance().getUserHome(event.refGetPath(), pm);
        for(StringBuilder alarm: alarms) {
        	Map<String,ICalField> alarmFields = this.parseICal(
        		new BufferedReader(new StringReader("BEGIN:VEVENT\n" + alarm + "\nEND:VEVENT")), 
        		new StringBuilder()
        	);
    		TimerQuery timerQuery = (TimerQuery)pm.newQuery(Timer.class);
    		timerQuery.thereExistsTarget().equalTo(event);
    		List<Timer> timers = userHome.getTimer(timerQuery);
    		if(timers.isEmpty()) {
    			try {
    				pm.currentTransaction().begin();
    	    		String summary = ICalField.getFieldValue("SUMMARY", alarmFields);
    	    		String description = ICalField.getFieldValue("DESCRIPTION", alarmFields);
    				Timer timer = pm.newInstance(Timer.class);
    				timer.setName(
    					summary == null 
    						? description == null 
    							? "Timer " + event.getName() 
    							: description 
    						: summary
    				);
    				timer.setTarget(event);
    				userHome.addTimer(
    					this.getUidAsString(),
    					timer
    				);
    				pm.currentTransaction().commit();
    				timers = Collections.singletonList(timer);
    			} catch(Exception e) {
    				new ServiceException(e).log();
    				try {
    					pm.currentTransaction().rollback();
    				} catch(Exception ignore) {}
    			}	
    		}
    		for(Timer timer: timers) {
    			try {
    				pm.currentTransaction().begin();
    	    		// Trigger
    				{
	    	    		String trigger = ICalField.getFieldValue("TRIGGER", alarmFields);
	    	    		if(trigger != null) {
	    	    			long minutes = -60;
	    	    			if(trigger.startsWith("-P")) {
	    	    				try {
	    	    					int factor = -1;
	    	    					if(trigger.endsWith("M")) {
	    	    						factor = -1;
	    	    					} else if(trigger.endsWith("H")) {
	    	    						factor = -60;
	    	    					} else if(trigger.endsWith("D")) {
	    	    						factor = -24 * 60;
	    	    					}
	    	    					minutes = factor * Integer.valueOf(trigger.substring(trigger.startsWith("-PT") ? 3 : 2, trigger.length()-1));
	    	    				} catch(Exception ignore) {}
	    	    			}
	    	    			timer.setTimerStartAt(new Date(event.getScheduledStart().getTime() + minutes * 60000L));
	    	    		} else {
	    	    			// Set timer one hour before scheduled start by default
	    	    			timer.setTimerStartAt(new Date(event.getScheduledStart().getTime() - 3600000L));
	    	    		}
    				}
    	    		// Repeat
    	    		{
	    	    		String repeat = ICalField.getFieldValue("REPEAT", alarmFields);
	    				if(repeat != null) {
	    					timer.setTriggerRepeat(Integer.valueOf(repeat));
	    				} else {
	    					timer.setTriggerRepeat(1);
	    				}
    	    		}
    	    		// Duration
    	    		{
    	    			String duration = ICalField.getFieldValue("DURATION", alarmFields);
    	    			if(duration != null) {
	    	    			Integer minutes = 5;
	    	    			if(duration.startsWith("PT") && duration.endsWith("M")) {
	    	    				try {
	    	    					minutes = Integer.valueOf(duration.substring(2, duration.length()-1));
	    	    				} catch(Exception ignore) {}
	    	    			}
	    	    			timer.setTriggerIntervalMinutes(minutes); 	    				
    	    			} else {
    	    				timer.setTriggerIntervalMinutes(5);
    	    			}
    	    		}
    	    		timer.setLastTriggerAt(new Date());
    	    		// Disable in case X-MOZ-LASTACK is set. 
    	    		// This prevents an ever looping alarm pop-up
    				timer.setDisabled(
    					// Mozilla
    					calendar.indexOf("X-MOZ-LASTACK:") > 0 ||
    					// iOS
    					alarm.indexOf("ACKNOWLEDGED:") > 0
    				);
    				timer.setTimerState(new Short((short)10)); // open
    				timer.setTimerEndAt(new Date(event.getScheduledStart().getTime() + 3600000L));
    				pm.currentTransaction().commit();
    			} catch(Exception e) {
    				new ServiceException(e).log();
    				try {
    					pm.currentTransaction().rollback();
    				} catch(Exception ignore) {}
    			}
    		}
        }
    }

	/**
	 * Get UID of this activity resource.
	 * 
	 * @param event
	 * @return
	 */
	public String getUid(
    	String event
    ) {
    	String uid = null;
    	if(event.indexOf("UID:") > 0) {
    		int start = event.indexOf("UID:");
    		int end = event.indexOf("\n", start);
    		if(end > start) {
    			uid = event.substring(start + 4, end).trim();
    		}
    	}    	
    	return uid;
    }

	/**
	 * Validate event UIDs when printing a calendar. Override this method
	 * for custom-specific behavior. Returns by default false. If true,
	 * the UID of an event is compared to its externalLink of type ICAL_SCHEMA.
	 * In case they do not match, a warning is logged. Turning this flag on
	 * results in significantly more database round-trips.
	 * 
	 * @return
	 */
	public boolean validateUid(
	) {
		return false;
	}

	/**
	 * Print activity as ICAL VCALENDAR.
	 * 
	 * @param pw
	 * @param activity
	 * @param queryHelper
	 * @param runAs
	 * @param eventsOnly
	 * @param req
	 * @param accessUrlContext
	 */
	public void printCalendar(
		PrintWriter pw,
		Activity activity,
		ActivityQueryHelper queryHelper,
		String runAs,
		boolean eventsOnly,
		HttpServletRequest req,
		String accessUrlContext
	) {
    	PersistenceManager pm = JDOHelper.getPersistenceManager(activity);
    	String ical = activity.getIcal();
    	// In case of recurring activities collect activities 
    	// which are member of the same recurrence
    	List<Activity> events = new ArrayList<Activity>();
    	events.add(activity);
    	String uid = this.getUid(ical);
    	if((uid != null) && (ical.indexOf("RRULE:") > 0) && queryHelper != null) {
        	ActivityQuery relatedActivitiesQuery = (ActivityQuery)pm.newQuery(Activity.class);
        	relatedActivitiesQuery.thereExistsExternalLink().equalTo(
        		ICalendar.ICAL_SCHEMA + uid 
        	);
        	relatedActivitiesQuery.thereExistsExternalLink().startsWith(
        		ICalendar.ICAL_RECURRENCE_ID_SCHEMA
        	);
        	Collection<Activity> members = queryHelper.getFilteredActivities(relatedActivitiesQuery);
        	events.addAll(members);
    	}
        if(!eventsOnly) {
	    	pw.println("BEGIN:VCALENDAR");
	        pw.println("PRODID:" + ICalendar.PROD_ID);
	        pw.println("VERSION:2.0");
	        pw.println("CALSCALE:GREGORIAN");
        }
    	for(Activity event: events) {
	        ical = event.getIcal();
	        // Obfuscate event if we are in impersonate mode and event is not PUBLIC
	        if(runAs != null && ical.indexOf("CLASS:PUBLIC") < 0) {
	        	// UID
	        	String oUID = null;
	        	int pos1 = ical.indexOf("UID");
	        	if(pos1 >= 0) {
		        	int pos2 = ical.indexOf("\n", pos1);
		        	if(pos2 > pos1) {
		        		oUID = ical.substring(pos1, pos2);
		        	}
	        	}
	        	// DTSTAMP
	        	String oDTSTAMP = null;
	        	pos1 = ical.indexOf("DTSTAMP");
	        	if(pos1 >= 0) {
		        	int pos2 = ical.indexOf("\n", pos1);
		        	if(pos2 > pos1) {
		        		oDTSTAMP = ical.substring(pos1, pos2);
		        	}
	        	}
	        	// ORGANIZER
	        	String oORGANIZER = null;
	        	pos1 = ical.indexOf("ORGANIZER");
	        	if(pos1 >= 0) {
		        	int pos2 = ical.indexOf("\n", pos1);
		        	if(pos2 > pos1) {
		        		oORGANIZER = ical.substring(pos1, pos2);
		        	}
	        	}
	        	// DTSTART
	        	String oDTSTART = null;
	        	pos1 = ical.indexOf("DTSTART");
	        	if(pos1 >= 0) {
		        	int pos2 = ical.indexOf("\n", pos1);
		        	if(pos2 > pos1) {
		        		oDTSTART = ical.substring(pos1, pos2);
		        	}
	        	}
	        	// DTEND
	        	String oDTEND = null;
	        	pos1 = ical.indexOf("DTEND");
	        	if(pos1 >= 0) {
		        	int pos2 = ical.indexOf("\n", pos1);
		        	if(pos2 > pos1) {
		        		oDTEND = ical.substring(pos1, pos2);
		        	}
	        	}
	        	// RRULE
	        	String oRRULE = null;
	        	pos1 = ical.indexOf("RRULE");
	        	if(pos1 >= 0) {
		        	int pos2 = ical.indexOf("\n", pos1);
		        	if(pos2 > pos1) {
		        		oRRULE = ical.substring(pos1, pos2);
		        	}
	        	}
	        	ical = 
        			"BEGIN:VEVENT\n" +        			
        			(oUID == null ? "" : oUID + "\n") +
        			"CLASS:CONFIDENTIAL\n" +
        			(oDTSTAMP == null ? "" : oDTSTAMP + "\n") +
        			(oORGANIZER == null ? "" : oORGANIZER + "\n") +
        			"SUMMARY:*** (" + runAs + ")\n" +
        			(oDTSTART == null ? "" : oDTSTART + "\n") +                		
                    (oDTEND == null ? "" : oDTEND + "\n") +          	
                    (oRRULE == null ? "" : oRRULE + "\n") +          	
                    "END:VEVENT\n";
	        }
	        uid = this.getUid(ical);
	        if(this.validateUid()) {
		        boolean externalLinkMatchesUid = false;
		        if(uid != null) {
			        for(String externalLink: event.getExternalLink()) {
			        	if(externalLink.startsWith(ICalendar.ICAL_SCHEMA) && externalLink.endsWith(uid)) {
			        		externalLinkMatchesUid = true;
			        		break;
			        	}
			        }
		        }
		        if(!externalLinkMatchesUid) {
		        	ServiceException e0 = new ServiceException(
		        		BasicException.Code.DEFAULT_DOMAIN,
		        		BasicException.Code.ASSERTION_FAILURE,
		        		"Mismatch of activity's external link and ical's UID. Use updateIcal() to fix event.",
		        		new BasicException.Parameter("activity", activity.refGetPath().toXRI()),
		        		new BasicException.Parameter("externalLink", activity.getExternalLink()),
		        		new BasicException.Parameter("uid", uid),
		        		new BasicException.Parameter("ical", ical)
		        	);
		        	SysLog.warning("Mismatch of activity's external link and ical's UID. Use updateIcal() to fix event.", Arrays.asList(activity.refGetPath().toString(), activity.getActivityNumber()));
		        	SysLog.detail(e0.getMessage(), e0.getCause());
		        }
	        }
	        ical = ical.replace("\r\n", "\n"); // Remove \r just in case
	        // VEVENT
	        if(ical.indexOf("BEGIN:VEVENT") >= 0) {
	            int start = ical.indexOf("BEGIN:VEVENT");
	            int end = ical.indexOf("END:VEVENT");
	            if(end < 0 || start < 0 || end < start) {
	            	SysLog.log(Level.WARNING, "ICAL {0} of activity {1} has bad format and will be ignored", ical, event.refGetPath().toXRI());
	            } else {
		            String vevent = ical.substring(start, end);
		    		String url = null;
		    		try {
		    			url = Base.getInstance().getAccessUrl(req, accessUrlContext, event);
		    		} catch(Exception e) {}
		        	if((start = vevent.indexOf("SUMMARY:")) > 0) {
	        			end = vevent.indexOf("\n", start);
	        			vevent = 
	        				vevent.substring(0, start) + 
	        				vevent.substring(start, end).replace(",", "\\,") + 
	        				vevent.substring(end); 	        			
		        	}
		        	if((start = vevent.indexOf("DESCRIPTION:")) > 0) {
	        			end = vevent.indexOf("\n", start);
	        			vevent = 
	        				vevent.substring(0, start) + 
	        				vevent.substring(start, end).replace(",", "\\,") + 
	        				vevent.substring(end); 	        			
		        	}
		        	// According to RFC 2445, Section 3.5 Security Considerations
		        	// the "Organizer" is the only person authorized to make changes 
		        	// to an existing event. Removing the organizer here makes the 
		        	// event editable on most CalDAV clients. It makes more sense to
		        	// handle event updates by access control.
		        	// by the access control of openCRX.
		        	if((start = vevent.indexOf("ORGANIZER:")) > 0) {
	        			end = vevent.indexOf("\n", start);
		        		vevent = 
		        			vevent.substring(0, start) +
		        			vevent.substring(end + 1);	
		        	}
		            pw.print(vevent);
		            SysLog.detail(vevent);
		            if(vevent.indexOf("TRANSP:") < 0) {
			        	try {
			        		String transp = "OPAQUE";
			        		if(transp != null) {
			        			pw.println("TRANSP:" + transp);
			        		}
			        	} catch(Exception e) {}
		            }
		            if(vevent.indexOf("URL:") < 0) {
		            	if(url != null) {
		            		pw.println("URL:" + url);
		            	}
		            }
		            try {
		            	this.printAlarms(pw, event);
		            } catch(Exception ignore) {}
		            pw.println("END:VEVENT");
	            }
	        } else if(ical.indexOf("BEGIN:VTODO") >= 0) {
		        // VTODO
	            int start = ical.indexOf("BEGIN:VTODO");
	            int end = ical.indexOf("END:VTODO");
	            if(end < 0 || start < 0 || end < start) {
	            	SysLog.log(Level.WARNING, "ICAL {0} of activity {1} has bad format and will be ignored", ical, event.refGetPath().toXRI());
	            } else {	            
		            String vtodo = ical.substring(start, end);
		            String url = null;
		            try {
		            	url = Base.getInstance().getAccessUrl(req, accessUrlContext, event);
		            } catch(Exception e) {}
		        	if((start = vtodo.indexOf("SUMMARY:")) > 0) {
	        			end = vtodo.indexOf("\n", start);
	        			vtodo = 
	        				vtodo.substring(0, start) + 
	        				vtodo.substring(start, end).replace(",", "\\,") + 
	        				vtodo.substring(end); 	        			
		        	}
		        	if((start = vtodo.indexOf("DESCRIPTION:")) > 0) {
	        			end = vtodo.indexOf("\n", start);
	        			vtodo = 
	        				vtodo.substring(0, start) + 
	        				vtodo.substring(start, end).replace(",", "\\,") + 
	        				vtodo.substring(end); 	        			
		        	}
		            pw.print(vtodo);
		            SysLog.detail(vtodo);
		            if(vtodo.indexOf("URL:") < 0) {
		            	if(url != null) {
		            		pw.println("URL:" + url);
		            	}
		            }
		            try {
		            	this.printAlarms(pw, event);
		            } catch(Exception ignore) {}
		            pw.println("END:VTODO");
	            }
	        }
    	}
    	if(!eventsOnly) {
    		pw.print("END:VCALENDAR");
    	}
	}

	//-------------------------------------------------------------------------
    // Members
    //-------------------------------------------------------------------------
    public static final String DATETIME_FORMAT =  "yyyyMMdd'T'HHmmss";
    public static final String DATE_FORMAT =  "yyyyMMdd";
    public static final String PROD_ID = "//OPENCRX//V2//EN";
    public static final String MIME_TYPE = "text/calendar";
    public static final String FILE_EXTENSION = ".ics";
    public static final String ICAL_SCHEMA = "ICAL:";    
    public static final String ICAL_RECURRENCE_ID_SCHEMA = "ICAL-RECURRENCE-ID:";    
    public static final Short USAGE_EMAIL_PRIMARY = new Short((short)300);
    // If X-OPENCRX-RENDER-ALARMS is set to TRUE printAlarms() retrieves assigned timers and
    // renders them as ALARMS. The flag is auto-calculated on activity updates: it is set
    // to TRUE if the activity has assigned timers, otherwise it is set to FALSE. The flag's
    // main purpose is performance tuning.
    public static final String X_OPENCRX_RENDER_ALARMS = "X-OPENCRX-RENDER-ALARMS";
    public static final String X_OPENCRX_RENDER_ALARMS_TRUE = X_OPENCRX_RENDER_ALARMS + ":TRUE";
    public static final String X_OPENCRX_RENDER_ALARMS_FALSE = X_OPENCRX_RENDER_ALARMS + ":FALSE";

    public static final int MIME_TYPE_CODE = 4;

    public static final short ICAL_TYPE_VTODO = 2;
    public static final short ICAL_TYPE_VEVENT = 1;
    public static final short ICAL_TYPE_NA = 0;
    
}

//--- End of File -----------------------------------------------------------
