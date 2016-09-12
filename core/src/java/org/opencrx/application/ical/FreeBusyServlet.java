/*
 * ====================================================================
 * Project:     openCRX/Core, http://www.opencrx.org/
 * Description: FreeBusyServlet
 * Owner:       CRIXP AG, Switzerland, http://www.crixp.com
 * ====================================================================
 *
 * This software is published under the BSD license
 * as listed below.
 * 
 * Copyright (c) 2004-2013, CRIXP Corp., Switzerland
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
import java.text.ParseException;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.Date;
import java.util.GregorianCalendar;
import java.util.List;

import javax.jdo.PersistenceManager;
import javax.jdo.PersistenceManagerFactory;
import javax.naming.NamingException;
import javax.servlet.ServletConfig;
import javax.servlet.ServletException;
import javax.servlet.http.HttpServlet;
import javax.servlet.http.HttpServletRequest;
import javax.servlet.http.HttpServletResponse;

import org.opencrx.kernel.activity1.cci2.ActivityQuery;
import org.opencrx.kernel.activity1.jmi1.Absence;
import org.opencrx.kernel.activity1.jmi1.Activity;
import org.opencrx.kernel.activity1.jmi1.ExternalActivity;
import org.opencrx.kernel.activity1.jmi1.Meeting;
import org.opencrx.kernel.activity1.jmi1.PhoneCall;
import org.opencrx.kernel.backend.ICalendar;
import org.opencrx.kernel.backend.UserHomes;
import org.opencrx.kernel.generic.SecurityKeys;
import org.opencrx.kernel.home1.cci2.EMailAccountQuery;
import org.opencrx.kernel.home1.cci2.UserHomeQuery;
import org.opencrx.kernel.home1.jmi1.EMailAccount;
import org.opencrx.kernel.home1.jmi1.UserHome;
import org.opencrx.kernel.utils.ActivityQueryHelper;
import org.opencrx.kernel.utils.ComponentConfigHelper;
import org.opencrx.kernel.utils.Utils;
import org.openmdx.base.exception.ServiceException;
import org.openmdx.base.persistence.cci.PersistenceHelper;
import org.openmdx.kernel.log.SysLog;
import org.w3c.format.DateTimeFormat;

/**
 * FreeBusyServlet
 *
 */
public class FreeBusyServlet extends HttpServlet {

    /**
     * RRule
     *
     */
    protected static class RRule {
    
        protected Date getUtcDate(
            String dateTime
        ) throws ParseException {
            Date date = null;
            if(dateTime.endsWith("Z")) {
                if(dateTime.length() == 16) {
                    date = DateTimeFormat.BASIC_UTC_FORMAT.parse(dateTime.substring(0, 15) + ".000Z");
                }
                else {
                    date = DateTimeFormat.BASIC_UTC_FORMAT.parse(dateTime);
                }
            }
            else if(dateTime.length() == 8) {
                date = DateTimeFormat.BASIC_UTC_FORMAT.parse(dateTime + "T000000.000Z");
            }
            return date;
        }

        public void parse(
            String rrule
        ) {
            if(rrule.startsWith("RRULE:")) {
                int end = 6;
                while((end < rrule.length()) && !Character.isWhitespace(rrule.charAt(end))) {
                    end++;                    
                }
                String[] attrs = rrule.substring(6, end).split(";");
                for(int i = 0; i < attrs.length; i++) {
                    if(attrs[i].startsWith("FREQ=")) {                        
                        this.freq = attrs[i].substring(5);
                    }
                    else if(attrs[i].startsWith("COUNT=")) {                        
                        this.count = Integer.valueOf(attrs[i].substring(6));
                    }
                    else if(attrs[i].startsWith("INTERVAL=")) {          
                        this.interval = Integer.valueOf(attrs[i].substring(9));                        
                    }
                    else if(attrs[i].startsWith("UNTIL=")) {
                    	try {
                    		this.until = getUtcDate(attrs[i].substring(6));
                    	} catch(Exception e) {}
                    }
                }
            }
        }
        
        public String getFreq(
        ) {
            return this.freq;
        }
        
        public int getCount(
        ) {
            return this.count;
        }
        
        public int getInterval(
        ) {
            return this.interval;            
        }
        
        public Date getUntil(
        ) {
        	return this.until;
        }
        
        private String freq = "DAILY";
        private int count = 1;
        private int interval = 1;
        private Date until = null;
    }
    
    /* (non-Javadoc)
     * @see javax.servlet.GenericServlet#init(javax.servlet.ServletConfig)
     */
    @Override
    public void init(
        ServletConfig config            
    ) throws ServletException {
        super.init();        
        if(this.pmf == null) {                    
            try {
                Utils.getModel();
                this.pmf = Utils.getPersistenceManagerFactory();
            }
            catch (NamingException e) {
                throw new ServletException( 
                    "Can not get the initial context", 
                    e
                );                
            }
            catch(ServiceException e) {
                throw new ServletException( 
                    "Can not get persistence manager", 
                    e
                );                
            }   
        }            
    }
    
    /**
     * Get persistence manager for root user.
     * 
     * @return
     */
    protected PersistenceManager getRootPersistenceManager(
    ) {
        return this.pmf.getPersistenceManager(
            SecurityKeys.ROOT_PRINCIPAL,
            null
        );    	
    }
    
    /**
     * Get configuration for this component.
     * 
     * @param providerName
     * @param rootPm
     * @return
     */
    protected org.opencrx.kernel.admin1.jmi1.ComponentConfiguration getComponentConfiguration(
        String providerName,
        PersistenceManager rootPm
    ) {
		return ComponentConfigHelper.getComponentConfiguration(
			CONFIGURATION_ID,
			providerName,
			rootPm,
			false,
			null
		);
    }
    
    /**
     * Get activities query helper.
     * 
     * @param pm
     * @param filterId
     * @param isDisabledFilter
     * @return
     */
    protected ActivityQueryHelper getActivitiesQueryHelper(
        PersistenceManager pm,
        String filterId,
        String isDisabledFilter
    ) {
        ActivityQueryHelper activitiesHelper = new ActivityQueryHelper(pm);
        if(filterId != null) {
            try {
                activitiesHelper.parseQueryId(                        
                    (filterId.startsWith("/") ? "" : "/") + filterId
                );
                activitiesHelper.parseDisabledFilter(
                   isDisabledFilter
                );
            } catch(Exception ignore) {}
        }        
        return activitiesHelper;
    }

    /**
     * Return true if string matches token.
     * 
     * @param token
     * @param strings
     * @return
     */
    protected boolean matches(
    	String token,
    	List<String> strings
    ) {
    	for(String str: strings) {
    		if(token.indexOf(str) >= 0) {
    			return true;
    		}
    	}
    	return false;
    }
    
    /* (non-Javadoc)
     * @see javax.servlet.http.HttpServlet#doGet(javax.servlet.http.HttpServletRequest, javax.servlet.http.HttpServletResponse)
     */
    @Override
    protected void doGet(
        HttpServletRequest req, 
        HttpServletResponse resp
    ) throws ServletException, IOException {
        String id = req.getParameter(PARAMETER_NAME_ID);
        if(id == null) {
    		resp.setStatus(HttpServletResponse.SC_BAD_REQUEST);
    		return;
        } 
       	String[] ids = id.split("/");
    	if(ids.length < 2) {
    		resp.setStatus(HttpServletResponse.SC_BAD_REQUEST);
    		return;
    	}
    	String providerName = ids[0];
    	String segmentName = ids[1];
    	String user = null;
    	for(int i = 0; i < ids.length; i++) {
    		if("home".equals(ids[i]) || "userhome".equals(ids[i])) {
    			user = ids[i+1];
    			break;
    		}
    	}
    	if(user == null) {
    		user = req.getParameter(PARAMETER_NAME_USER);
    		if(user == null) {
        		resp.setStatus(HttpServletResponse.SC_BAD_REQUEST);
        		return;    			
    		}
    	}    	
    	PersistenceManager pm = this.pmf.getPersistenceManager(
    		SecurityKeys.ADMIN_PRINCIPAL + SecurityKeys.ID_SEPARATOR + segmentName,
    		null
    	);
    	org.opencrx.kernel.home1.jmi1.Segment userHomeSegment = null;
    	try {
    		userHomeSegment = UserHomes.getInstance().getUserHomeSegment(pm, providerName, segmentName);
    	} catch(Exception e) {}
    	UserHome userHome = null;
    	if(user.indexOf("@") > 0) {
    		UserHomeQuery query = (UserHomeQuery)pm.newQuery(UserHome.class);
    		query.thereExistsEMailAccount().name().equalTo(user);
    		List<UserHome> userHomes = userHomeSegment.getUserHome(query);
    		if(!userHomes.isEmpty()) {
    			userHome = userHomes.iterator().next();
    		}    		
    	} else {
    		userHome = userHomeSegment.getUserHome(user);
    	}
    	String userId = null;
    	List<String> userEMails = new ArrayList<String>();
    	if(userHome != null) {
    		userId = userHome.refGetPath().getBase();
			EMailAccountQuery query = (EMailAccountQuery)pm.newQuery(EMailAccount.class);
			query.thereExistsIsActive().isTrue();
			query.thereExistsIsDefault().isTrue();
			List<EMailAccount> emailAccounts = userHome.getEMailAccount(query);
			for(EMailAccount emailAccount: emailAccounts) {
				userEMails.add(emailAccount.getName());
			}
    	}
    	if(userId == null || userEMails.isEmpty()) {
    		SysLog.warning("Invalid user", Arrays.asList(id, user));
    	}
        pm.close();
    	pm = this.pmf.getPersistenceManager(
    		userId,
    		null
    	);
        String isDisabledFilter = req.getParameter(PARAMETER_NAME_DISABLED);
        ActivityQueryHelper activitiesHelper = this.getActivitiesQueryHelper(
            pm, 
            id,
            isDisabledFilter
        );
        // Return all activities in FreeBusy format
        if((req.getRequestURI().endsWith("/freebusy"))) {        	            
            resp.setStatus(HttpServletResponse.SC_OK);
            resp.setCharacterEncoding("UTF-8");    
            resp.setContentType("text/plain");
            PrintWriter p = resp.getWriter();
            boolean isTypeICS = RESOURCE_FORMAT_ICS.equals(req.getParameter(PARAMETER_NAME_TYPE));
            Date dtStart = new Date(System.currentTimeMillis() - 7*86400000L);
            Date dtEnd = new Date(System.currentTimeMillis() + 60*86400000L);
            p.write("BEGIN:VCALENDAR\n");
            p.write("PRODID:-" + ICalendar.PROD_ID + "\n");
            p.write("VERSION:2.0\n");
            p.write("METHOD:PUBLISH\n");
            if(isTypeICS) {
            	//
            } else {
            	p.write("BEGIN:VFREEBUSY\n");
                p.write("ORGANIZER:" + (userEMails.isEmpty() ? activitiesHelper.getCalendarName() : userEMails.get(0)) + "\n");
	            p.write("DTSTAMP:" + ActivityQueryHelper.formatDateTime(ActivityQueryHelper.getActivityGroupModifiedAt(activitiesHelper.getActivityGroup())) + "\n");
	            p.write("DTSTART:" + ActivityQueryHelper.formatDateTime(dtStart) + "\n");
	            p.write("DTEND:" + ActivityQueryHelper.formatDateTime(dtEnd) + "\n");
            }
            ActivityQuery activityQuery = (ActivityQuery)pm.newQuery(Activity.class);
            PersistenceHelper.setClasses(activityQuery, Meeting.class, PhoneCall.class, Absence.class, ExternalActivity.class);
            if(activitiesHelper.isDisabledFilter()) {
                activityQuery.thereExistsDisabled().isTrue();                    
            }
            else {
                activityQuery.forAllDisabled().isFalse();                    
            }
            activityQuery.ical().isNonNull();
            activityQuery.thereExistsScheduledStart().lessThanOrEqualTo(dtEnd);
            activityQuery.thereExistsScheduledEnd().greaterThanOrEqualTo(dtStart);
            activityQuery.orderByScheduledStart().ascending();
            for(Activity activity: activitiesHelper.getFilteredActivities(activityQuery)) {            	
                String ical = activity.getIcal();
            	if(ical.indexOf("TRANSP:TRANSPARENT") < 0) {
            		boolean isBusy = false;
            		String[] tokens = ical.split("\n");
            		for(String token: tokens) {
            			if(token.indexOf("ORGANIZER") >= 0 && this.matches(token, userEMails)) {
            				isBusy = true;
            				break;
            			} else if(token.indexOf("ATTENDEE") >= 0 && this.matches(token, userEMails) && token.indexOf("PARTSTAT=DECLINED") < 0) {
            				isBusy = true;
            				break;
            			}
            		}
            		if(isBusy) {
		                RRule rrule = new RRule();
		                if((ical != null) && (ical.indexOf("RRULE:") > 0)) {
		                    rrule.parse(ical.substring(ical.indexOf("RRULE:")));
		                }
		                GregorianCalendar scheduledStart = new GregorianCalendar();
		                scheduledStart.setTime(activity.getScheduledStart());
		                GregorianCalendar scheduledEnd = new GregorianCalendar();
		                scheduledEnd.setTime(activity.getScheduledEnd());
		                int i = 0;
		                while(
		                	(rrule.getUntil() != null && scheduledStart.getTime().compareTo(rrule.getUntil()) <= 0) ||
		                    (i < rrule.getCount())
		                ) {
		                	if(isTypeICS) {
		                		p.write("BEGIN:VEVENT\n");
		                		p.write("UID:" + activity.refGetPath().getBase() + "-" + i + "\n");
		                		p.write("CLASS:PUBLIC\n");
			    	            p.write("DTSTAMP:" + ActivityQueryHelper.formatDateTime(ActivityQueryHelper.getActivityGroupModifiedAt(activitiesHelper.getActivityGroup())) + "\n");
			                    p.write("ORGANIZER:" + (userEMails.isEmpty() ? activitiesHelper.getCalendarName() : userEMails.get(0)) + "\n");
			                    p.write("SUMMARY:***\n");
			                    if(activity.isAllDayEvent()) {
				                    p.write("DTSTART;VALUE=DATE:" + ActivityQueryHelper.formatDate(scheduledStart.getTime()) + "\n");                		
				                    p.write("DTEND;VALUE=DATE:" + ActivityQueryHelper.formatDate(scheduledEnd.getTime()) + "\n");
			                    } else {
				                    p.write("DTSTART:" + ActivityQueryHelper.formatDateTime(scheduledStart.getTime()) + "\n");                		
				                    p.write("DTEND:" + ActivityQueryHelper.formatDateTime(scheduledEnd.getTime()) + "\n");	                    	
			                    }
		                		p.write("END:VEVENT\n");
		                	} else {
		               			p.write("FREEBUSY:" + ActivityQueryHelper.formatDateTime(scheduledStart.getTime()) + "/" + ActivityQueryHelper.formatDateTime(scheduledEnd.getTime()) + "\n");                			
		                	}
		                    if("DAILY".equals(rrule.getFreq())) {
		                        scheduledStart.add(GregorianCalendar.DAY_OF_MONTH, rrule.getInterval());
		                        scheduledEnd.add(GregorianCalendar.DAY_OF_MONTH, rrule.getInterval());
		                    } else if("WEEKLY".equals(rrule.getFreq())) {
		                        scheduledStart.add(GregorianCalendar.WEEK_OF_YEAR, rrule.getInterval());
		                        scheduledEnd.add(GregorianCalendar.WEEK_OF_YEAR, rrule.getInterval());                                    
		                    } else if("MONTHLY".equals(rrule.getFreq())) {
		                        scheduledStart.add(GregorianCalendar.MONTH, rrule.getInterval());
		                        scheduledEnd.add(GregorianCalendar.MONTH, rrule.getInterval());                                                                        
		                    } else if("YEARLY".equals(rrule.getFreq())) {
		                        scheduledStart.add(GregorianCalendar.YEAR, rrule.getInterval());
		                        scheduledEnd.add(GregorianCalendar.YEAR, rrule.getInterval());                                                                                                            
		                    }
		                    i++;
		                }
            		}
            	}
            }
            if(isTypeICS) {
            	//
            } else {
            	p.write("END:VFREEBUSY\n");
            }
            p.write("END:VCALENDAR\n");
            p.flush();
        } else {
            super.doGet(req, resp);                
        }
        try {
            pm.close();            
        } catch(Exception ignore) {}
    }
    
    //-----------------------------------------------------------------------
    // Members
    //-----------------------------------------------------------------------
    private static final long serialVersionUID = 4746783518992145105L;
    protected final static String CONFIGURATION_ID = "ICalServlet";

    protected final static String PARAMETER_NAME_ID = "id";
    protected final static String PARAMETER_NAME_DISABLED = "disabled";
    protected final static String PARAMETER_NAME_USER = "user";
    protected final static String PARAMETER_NAME_TYPE = "type";
    protected final static String PARAMETER_NAME_RESOURCE = "resource";

    protected final static String RESOURCE_FORMAT_ICS = "ics";

    protected static final String PROPERTY_MAX_ACTIVITIES = "maxActivities";
    protected static final int DEFAULT_MAX_ACTIVITIES = 500;
    
    protected PersistenceManagerFactory pmf = null;
    
}
