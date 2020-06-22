/*
 * ====================================================================
 * Project:     openCRX/CalDAV, http://www.opencrx.org/
 * Description: ICalServlet
 * Owner:       CRIXP AG, Switzerland, http://www.crixp.com
 * ====================================================================
 *
 * This software is published under the BSD license
 * as listed below.
 * 
 * Copyright (c) 2004-2008, CRIXP Corp., Switzerland
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
package org.opencrx.application.ical;

import java.io.IOException;
import java.io.PrintWriter;
import java.text.SimpleDateFormat;
import java.util.Calendar;
import java.util.Collection;
import java.util.Date;
import java.util.GregorianCalendar;
import java.util.UUID;

import javax.jdo.PersistenceManager;
import javax.servlet.ServletException;
import javax.servlet.http.HttpServletRequest;
import javax.servlet.http.HttpServletResponse;

import org.opencrx.kernel.account1.cci2.ContactQuery;
import org.opencrx.kernel.account1.jmi1.Account;
import org.opencrx.kernel.account1.jmi1.Contact;
import org.opencrx.kernel.activity1.cci2.ActivityQuery;
import org.opencrx.kernel.activity1.jmi1.Activity;
import org.opencrx.kernel.backend.Accounts;
import org.opencrx.kernel.backend.Base;
import org.opencrx.kernel.backend.ICalendar;
import org.opencrx.kernel.utils.AccountQueryHelper;
import org.opencrx.kernel.utils.ActivityQueryHelper;
import org.opencrx.kernel.utils.ComponentConfigHelper;
import org.openmdx.base.exception.ServiceException;
import org.openmdx.base.jmi1.BasicObject;
import org.openmdx.base.text.conversion.UUIDConversion;
import org.openmdx.kernel.id.UUIDs;
import org.openmdx.kernel.log.SysLog;
import org.openmdx.portal.servlet.Action;
import org.openmdx.portal.servlet.WebKeys;
import org.openmdx.portal.servlet.action.SelectObjectAction;

/**
 * ICalServlet
 *
 */
public class ICalServlet extends FreeBusyServlet {

	/**
	 * CalendarType
	 *
	 */
	public enum CalendarType {

		BDAYS("/bdays"),
		ANNIVERSARIES("/anniversaries"),
		DATESOFDEATH("/datesofdeath");

		private CalendarType(
			String path
		) {
			this.path = path;
		}
		
		public String getPath() {
			return path;
		}

		private final String path;
	}

    /**
     * Returns true if date of given calendar is valid for given year.
     * 
     * @param contact
     * @param calendarType
     * @param year
     * @return
     */
    protected boolean acceptDate(
    	Contact contact,
    	CalendarType calendarType,
		int year
    ) {
    	GregorianCalendar dateOfBirth = null;
    	GregorianCalendar anniversary = null;
		GregorianCalendar dateOfDeath = null;
		GregorianCalendar dateOfBirthInYear = null;
		GregorianCalendar anniversaryInYear = null;
    	if(contact.getBirthdate() != null) {
    		dateOfBirth = new GregorianCalendar();
    		dateOfBirth.setTime(contact.getBirthdate());
    	}
    	if(contact.getAnniversary() != null) {
    		anniversary = new GregorianCalendar();
    		anniversary.setTime(contact.getAnniversary());
    	}
		if(contact.getDateOfDeath() != null) {
			dateOfDeath = new GregorianCalendar();
			dateOfDeath.setTime(contact.getDateOfDeath());
			if(dateOfBirth != null) {
				dateOfBirthInYear = (GregorianCalendar)dateOfBirth.clone();
				dateOfBirthInYear.set(Calendar.YEAR, year);
			}
			if(anniversary != null) {
				anniversaryInYear = (GregorianCalendar)anniversary.clone();
				anniversaryInYear.set(Calendar.YEAR, year);
			}
		}
    	switch(calendarType) {
	    	case BDAYS:
	    		return
	    			dateOfBirth != null && 
	    			dateOfBirth.get(Calendar.YEAR) <= year /*&&
	    			(dateOfDeath == null || dateOfBirthInYear.compareTo(dateOfDeath) <= 0)*/;
	    	case ANNIVERSARIES:
	    		return
	    			anniversary != null && 
	    			anniversary.get(Calendar.YEAR) <= year /*&&
	    			(dateOfDeath == null || anniversaryInYear.compareTo(dateOfDeath) <= 0)*/;
	    	case DATESOFDEATH:
	    		return
	    			dateOfDeath != null && 
	    			dateOfDeath.get(Calendar.YEAR) <= year;
	    	default:
	    		return false;
    	}
	}

    /**
     * Get persistence manager.
     * 
     * @param req
     * @return
     */
    protected PersistenceManager getPersistenceManager(
        HttpServletRequest req
    ) {
        return req.getUserPrincipal() == null ? null :
            this.pmf.getPersistenceManager(
                req.getUserPrincipal().getName(),
                null
            );
    }
    
    /**
     * Get accounts helper.
     * 
     * @param pm
     * @param filterId
     * @param isDisabledFilter
     * @return
     */
    protected AccountQueryHelper getAccountsHelper(
        PersistenceManager pm,
        String filterId,
        String isDisabledFilter
    ) {
    	AccountQueryHelper accountsHelper = new AccountQueryHelper(pm);
        if(filterId != null) {
            try {
            	accountsHelper.parseQueryId(                        
                    (filterId.startsWith("/") ? "" : "/") + filterId
                );
            }
            catch(Exception  e) {}
        }        
        return accountsHelper;
    }
    
    /**
     * Get access URL for given activity.
     * 
     * @param req
     * @param activity
     * @return
     */
    protected String getActivityUrl(        
        HttpServletRequest req,
        Activity activity
    ) {
        Action selectActivityAction = 
            new Action(
                SelectObjectAction.EVENT_ID, 
                new Action.Parameter[]{
                    new Action.Parameter(Action.PARAMETER_OBJECTXRI, activity.refMofId())
                },
                "",
                true
            );        
        return 
        	req.getContextPath().replace("-ical-", "-core-") + "/" + 
        	WebKeys.SERVLET_NAME + 
        	"?event=" + SelectObjectAction.EVENT_ID + 
        	"&amp;parameter=" + selectActivityAction.getParameterEncoded(); 
    }
    
    /**
     * Print given calendar of given type for given accounts.
     * 
     * @param calendarType
     * @param accountsHelper
     * @param req
     * @param resp
     * @throws ServiceException
     */
    protected void printCalendar(
    	CalendarType calendarType,
    	AccountQueryHelper accountsHelper,
        HttpServletRequest req, 
        HttpServletResponse resp    	
    ) throws IOException {
    	PersistenceManager pm = accountsHelper.getPersistenceManager();
    	final int YEARS_BEFORE_SELECTED_YEAR = 1;
    	final int YEARS_AFTER_SELECTED_YEAR = 1;
		// year
		GregorianCalendar now = new GregorianCalendar();
		int year = now.get(GregorianCalendar.YEAR);
		if(req.getParameter("year") != null) {
			try {
				year = Integer.parseInt(req.getParameter("year"));
			} catch (Exception e) {}
		}
		// alarm
		boolean createAlarm = req.getParameter("alarm") != null ? Boolean.valueOf(req.getParameter("alarm")) :
			req.getParameter("ALARM") != null ? Boolean.valueOf(req.getParameter("ALARM")) :
				false;
		// icalType
		short icalType = ICalendar.ICAL_TYPE_VEVENT;
		if ((req.getParameter("icalType") != null) && (req.getParameter("icalType").compareTo("VTODO") == 0)) {
			icalType = ICalendar.ICAL_TYPE_VTODO;
		}
		// max
        int max = req.getParameter("max") != null ?
        	Integer.valueOf(req.getParameter("max")) :
        		DEFAULT_MAX_ACTIVITIES;
        // categories
        String categories = req.getParameter("categories") != null ? req.getParameter("categories") :
        	calendarType.name();
        // summaryPrefix
        String summaryPrefix = req.getParameter("summaryPrefix") != null ? req.getParameter("summaryPrefix") :
        	"";
        if(accountsHelper.getAccountSegment() != null) {
			SimpleDateFormat dateTimeFormatter = new SimpleDateFormat(ICalendar.DATETIME_FORMAT);
			dateTimeFormatter.setLenient(false);
			GregorianCalendar calendarEndAt = new GregorianCalendar();
			calendarEndAt.add(GregorianCalendar.YEAR, YEARS_AFTER_SELECTED_YEAR + 1);
			calendarEndAt.set(GregorianCalendar.MONTH, 0);
			calendarEndAt.set(GregorianCalendar.DAY_OF_MONTH, 1);
			calendarEndAt.set(GregorianCalendar.HOUR_OF_DAY, 0);
			calendarEndAt.set(GregorianCalendar.MINUTE, 0);
			calendarEndAt.set(GregorianCalendar.SECOND, 0);
			calendarEndAt.set(GregorianCalendar.MILLISECOND, 0);
			ContactQuery contactQuery = (ContactQuery)pm.newQuery(Contact.class);
			contactQuery.forAllDisabled().isFalse();
			switch(calendarType) {
				case BDAYS:
					contactQuery.thereExistsBirthdate().lessThanOrEqualTo(calendarEndAt.getTime());
					break;
				case ANNIVERSARIES:
					contactQuery.thereExistsAnniversary().lessThanOrEqualTo(calendarEndAt.getTime());
					break;
				case DATESOFDEATH:
					contactQuery.thereExistsDateOfDeath().lessThanOrEqualTo(calendarEndAt.getTime());
					break;
			}
        	Collection<Account> contacts = accountsHelper.getFilteredAccounts(contactQuery);
            resp.setCharacterEncoding("UTF-8");
            resp.setStatus(HttpServletResponse.SC_OK);
            resp.setContentType("text/calendar");                
            PrintWriter p = resp.getWriter();	    
            p.write("BEGIN:VCALENDAR\n");
            p.write("VERSION:2.0\n");
            p.write("PRODID:" + ICalendar.PROD_ID + "\n");
            p.write("CALSCALE:GREGORIAN\n");
            p.write("METHOD:PUBLISH\n");    
            int n = 0;
        	for(Account account: contacts) {
        		if(account instanceof Contact) {
        			Contact contact = (Contact)account;
					for(
						int i = -YEARS_BEFORE_SELECTED_YEAR; 
						i <= YEARS_AFTER_SELECTED_YEAR; 
						i++
					) {			
						int currentYear = year + i;
						if(this.acceptDate(contact, calendarType, currentYear)) {
						    UUID uid = null;
						    try {
						        uid = UUIDConversion.fromString(contact.refGetPath().getLastSegment().toString());
						    } catch(Exception e) {
						        uid = UUIDs.newUUID();
						    }
						    String lastModified = dateTimeFormatter.format(new Date());
						    String dtStart = null;
						    String dtEnd = null;
						    String dtDue = null;
						    String valarm = null;
						    String name = "";
						    try {
					  			name = contact.getFullName();
					    		String age = "";
					    		String description = "";
					    		GregorianCalendar date = new GregorianCalendar();
					    		switch(calendarType) {
						    		case BDAYS: {
						    			date = new GregorianCalendar();
							    		date.setTime(contact.getBirthdate());
							    		if(contact.getDateOfDeath() == null) {
								    		age = Integer.toString(currentYear - date.get(Calendar.YEAR));
								    		description = name + " *" + date.get(Calendar.YEAR) + " (" + age + ")";							    			
							    		} else {
							    			GregorianCalendar dateOfDeath = new GregorianCalendar();
							    			dateOfDeath.setTime(contact.getDateOfDeath());
								    		description = name + " " + date.get(Calendar.YEAR) + "-" + dateOfDeath.get(Calendar.YEAR) + " (+)";							    			
							    		}
							    		break;
						    		}
						    		case ANNIVERSARIES: {
						    			date = new GregorianCalendar();
							    		date.setTime(contact.getAnniversary());
							    		age = Integer.toString(currentYear - date.get(GregorianCalendar.YEAR));
							    		description = name + " #" + date.get(GregorianCalendar.YEAR) + " (" + age + ")";
							    		break;
						    		}
						    		case DATESOFDEATH: {
						    			date = new GregorianCalendar();
							    		date.setTime(contact.getDateOfDeath());
							    		age = Integer.toString(currentYear - date.get(GregorianCalendar.YEAR));
							    		description = name + " +" + date.get(GregorianCalendar.YEAR) + " (" + age + ")";
							    		break;
						    		}
					    		}
					    		date.set(Calendar.YEAR, currentYear);
					    		dtStart = "DTSTART;VALUE=DATE:" + (dateTimeFormatter.format(date.getTime())).substring(0, 8);
					    		if(createAlarm) {
									GregorianCalendar yesterday = new GregorianCalendar();
							        yesterday.add(GregorianCalendar.DAY_OF_MONTH, -1);
									if(yesterday.compareTo(date) <= 0) {
										valarm = "BEGIN:VALARM\n" +
								        "ACTION:DISPLAY\n" +
								        "DESCRIPTION:" + description + "\n" +
								        "TRIGGER;VALUE=DURATION:-P1D\n" +
								        "END:VALARM\n";
									}
					    		}
						        date.add(GregorianCalendar.DAY_OF_MONTH, 1);
						        dtEnd = "DTEND;VALUE=DATE:" + (dateTimeFormatter.format(date.getTime())).substring(0, 8);
						        dtDue = "DUE;VALUE=DATE:" + (dateTimeFormatter.format(date.getTime())).substring(0, 8);
						        String emailAddress = Accounts.getInstance().getPrimaryBusinessEMail(account, null);
						        String attendee = null;
						        if(emailAddress != null) {
									String fullName = contact.getFullName();
									if(fullName == null) {
										attendee = "ATTENDEE;CN=" + emailAddress + ";ROLE=REQ-PARTICIPANT;RSVP=TRUE:MAILTO:" + emailAddress + "\n";
									} else {
										attendee = "ATTENDEE;CN=\"" + fullName + " (" + emailAddress + ")\";ROLE=REQ-PARTICIPANT;RSVP=TRUE:MAILTO:" + emailAddress + "\n";
									}
						        }
						        p.write((icalType == ICalendar.ICAL_TYPE_VTODO ? "BEGIN:VTODO\n" : "BEGIN:VEVENT\n"));
						        p.write("UID:" + uid.toString() + "-" + age + "\n");
						        p.write("LAST-MODIFIED:" + lastModified.substring(0, 15) + "Z\n");
						        p.write(dtStart + "\n");
						        p.write(dtEnd + "\n");
						        p.write(dtDue + "\n");
						        p.write(valarm != null ? valarm : "");
						        p.write("CATEGORIES:" + categories + "\n");
						        p.write("DTSTAMP:" + (dateTimeFormatter.format(contact.getModifiedAt())).substring(0, 15) + "Z\n");
						        p.write("SUMMARY:" + (summaryPrefix == null || summaryPrefix.isEmpty() ? "" : summaryPrefix + " - ") + description + "\n");
						        p.write("DESCRIPTION:" + description + "\n");
						        p.write(attendee != null ? attendee : "");
						        p.write("PRIORITY:6\n");
						        p.write("STATUS:CONFIRMED\n");
						        p.write("CLASS:PUBLIC\n");
						        String url = Base.getInstance().getAccessUrl(req, "-ical-", contact);
					        	p.write("URL:" + url + "\n");
						        p.write(icalType == ICalendar.ICAL_TYPE_VTODO ? "END:VTODO\n" : "END:VEVENT\n");
					 		} catch (Exception e) {
					 			new ServiceException(e).log();
					 		}
						}
					}
				    n++;
                    if(n > max) break;
        		}
        	}
        	p.write("END:VCALENDAR\n");
            p.flush();
        }
    }

    /* (non-Javadoc)
     * @see org.opencrx.application.ical.FreeBusyServlet#doGet(javax.servlet.http.HttpServletRequest, javax.servlet.http.HttpServletResponse)
     */
    @Override
    protected void doGet(
        HttpServletRequest req, 
        HttpServletResponse resp
    ) throws ServletException, IOException {
        PersistenceManager pm = this.getPersistenceManager(req);
        PersistenceManager rootPm = this.getRootPersistenceManager();
        if(pm == null) {
            resp.setStatus(HttpServletResponse.SC_UNAUTHORIZED);
            return;
        }        
        String filterId = req.getParameter(PARAMETER_NAME_ID);
        String isDisabledFilter = req.getParameter(PARAMETER_NAME_DISABLED);
        if(req.getRequestURI().endsWith("/ical") || req.getRequestURI().endsWith("/activities")) {        
	        ActivityQueryHelper activitiesQueryHelper = this.getActivitiesQueryHelper(
	            pm, 
	            filterId,
	            isDisabledFilter
	        );
	        if(activitiesQueryHelper.getActivitySegment() != null) {
	            org.opencrx.kernel.admin1.jmi1.ComponentConfiguration componentConfiguration = 
	                this.getComponentConfiguration(
	                    activitiesQueryHelper.getActivitySegment().refGetPath().get(2),
	                    rootPm
	                );
	            String maxActivitiesValue = componentConfiguration == null ? 
	                null : 
	                ComponentConfigHelper.getComponentConfigProperty(
	                	PROPERTY_MAX_ACTIVITIES, 
	                	componentConfiguration
	                ).getStringValue();
	            int maxActivities = maxActivitiesValue == null ? 
	            	DEFAULT_MAX_ACTIVITIES : 
	                    Integer.valueOf(maxActivitiesValue);
	            // Return all activities in ICS format
	            if(
	                RESOURCE_FORMAT_ICS.equals(req.getParameter(PARAMETER_NAME_TYPE)) ||
	                RESOURCE_ACTIVITIES_ICS.equals(req.getParameter(PARAMETER_NAME_RESOURCE))
	            ) {
	                resp.setCharacterEncoding("UTF-8");
	                resp.setStatus(HttpServletResponse.SC_OK);
	                resp.setContentType("text/calendar");
	                ActivityQuery activityQuery = (ActivityQuery)pm.newQuery(Activity.class);
	                if(activitiesQueryHelper.isDisabledFilter()) {
	                    activityQuery.thereExistsDisabled().isTrue();                    
	                } else {
	                    activityQuery.forAllDisabled().isFalse();                    
	                }
	                activityQuery.ical().isNonNull();
	                // No recurring events here. They will be generated by printCalendar() below
	                activityQuery.forAllExternalLink().startsNotWith(ICalendar.ICAL_RECURRENCE_ID_SCHEMA);	                
	                PrintWriter p = resp.getWriter();
	                p.write("BEGIN:VCALENDAR\n");
	                p.write("VERSION:2.0\n");
	                p.write("PRODID:" + ICalendar.PROD_ID + "\n");
	                p.write("CALSCALE:GREGORIAN\n");
	                p.write("METHOD:PUBLISH\n");
	                
	                // Event serving as calendar guard. Required to allow
	                // creation of events in doPut
	                p.write("BEGIN:VEVENT\n");
	                BasicObject source = activitiesQueryHelper.getSource();
	                p.write("UID:" + (source == null ? "-" : source.refGetPath().getLastSegment().toString()) + "\n");
	                p.write("CLASS:PUBLIC\n");
	                p.write("DTSTART:19000101T000000Z\n");
	                p.write("DTEND:19000101T000000Z\n");
	                p.write("LAST-MODIFIED:19000101T000000Z\n");
	                p.write("DTSTAMP:19000101T000000Z\n");
	                p.write("SUMMARY:" + filterId + "\n");
	                p.write("END:VEVENT\n");
	                int n = 0;
	                for(Activity activity: activitiesQueryHelper.getFilteredActivities(activityQuery)) {
	                	try {
	                        SysLog.detail("VEVENT #", n);
	                    	ICalendar.getInstance().printCalendar(
	                    		p, 
	                    		activity, 
	                    		activitiesQueryHelper, 
	                    		null, // runAs 
	                    		true, // eventsOnly 
	                    		req, 
	                    		"-ical-"
	                    	);
	                	} catch(Exception e) {
	                		new ServiceException(e).log();
	                	}
	                    n++;
	                    if(n > maxActivities) break;
	                }
	                p.write("END:VCALENDAR\n");
	                p.flush();
	            }
	        } else {
                super.doGet(req, resp);
            }
        } else if(req.getRequestURI().endsWith(CalendarType.BDAYS.getPath())) {       	
        	this.printCalendar(
        		CalendarType.BDAYS,
        		this.getAccountsHelper(pm, filterId, isDisabledFilter),
        		req,
        		resp
        	);
        } else if(req.getRequestURI().endsWith(CalendarType.ANNIVERSARIES.getPath())) {
        	this.printCalendar(
        		CalendarType.ANNIVERSARIES,
        		this.getAccountsHelper(pm, filterId, isDisabledFilter),
        		req,
        		resp
        	);
        } else if(req.getRequestURI().endsWith(CalendarType.DATESOFDEATH.getPath())) {
        	this.printCalendar(
        		CalendarType.DATESOFDEATH,
        		this.getAccountsHelper(pm, filterId, isDisabledFilter),
        		req,
        		resp
        	);
	    } else {
	    	super.doGet(req, resp);
	    }        
        try {
        	if(pm != null) {
        		pm.close();
        	}
        	if(rootPm != null) {
        		rootPm.close();
        	}
        } catch(Exception e) {}
    }

    //-----------------------------------------------------------------------
    // Members
    //-----------------------------------------------------------------------
    private static final long serialVersionUID = 4746783518992145105L;
    
    protected final static String RESOURCE_ACTIVITIES_ICS = "activities.ics";
        
}
