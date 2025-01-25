<%@  page contentType="text/html;charset=UTF-8" language="java" pageEncoding="UTF-8" %><%
/**
 * ====================================================================
 * Project:	    openCRX/Core, http://www.opencrx.org/
 * Description: ScheduleEventWizard
 * Owner:	    the original authors.
 * ====================================================================
 *
 * This software is published under the BSD license
 * as listed below.
 * * Redistribution and use in source and binary forms, with or without
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
 * * Neither the name of the openCRX team nor the names of the contributors
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
%><%@ page session="true" import="
java.util.*,
java.io.*,
java.text.*,
org.openmdx.base.text.conversion.*,
org.openmdx.kernel.id.cci.*,
org.openmdx.kernel.id.*,
org.openmdx.base.accessor.jmi.cci.*,
org.openmdx.portal.servlet.*,
org.openmdx.portal.servlet.attribute.*,
org.openmdx.portal.servlet.component.*,
org.openmdx.portal.servlet.control.*,
org.openmdx.portal.servlet.wizards.*,
org.openmdx.base.naming.*,
org.openmdx.kernel.log.*
" %>
<%!

	public static final int NUM_SLOTS = 8;
	public static final int NUM_SLOTS_INITIALLY_VISIBLE = 3;
	public static final short IMPORTANCE_HIGH = 3;
	public static final short CODE_ACTIVITYLINKTYPE_RELATESTO = 6;
  public static final String TIME_MISSING = "0000.000Z@<";
	public static final String FORM_NAME = "ScheduleEventForm";
	public static final String SUBMIT_HANDLER = "javascript:$('Command').value=this.name;";
	public static final String SUBMIT_HANDLER_WITH_CHECK = SUBMIT_HANDLER + "return validateForm('" + FORM_NAME + "');";
	public static final String WIZARD_NAME = "ScheduleEventWizard.jsp";
	public static final String VOTE_FOR_EVENT_WIZARD_NAME = "VoteForEvent.jsp";
	public static final String ANCHOR_TOP = "Top";
	public static final String ANCHOR_CALENDAR = "Calendar";
	public static final String ANCHOR_PARTICIPANTS = "Participants";
	public static final String ANCHOR_VOTES = "Votes";
	public static final String ANCHOR_BOTTOM = "Bottom";
	public static final Map resourceBundles = new HashMap();

	public static Properties getResourceBundle(
		Locale locale,
		ServletContext servletContext
	) {
		String bundleId = locale.getLanguage() + "_" + locale.getCountry();
		Properties resourceBundle = (Properties)resourceBundles.get(bundleId);
		if(resourceBundle == null) {
			try {
				java.net.URL bundleURL = servletContext.getResource("/wizards/ScheduleEventWizard_" + bundleId + " + .xml");
				InputStream in = bundleURL.openStream();
				resourceBundle = new Properties();
				resourceBundle.loadFromXML(in);
				in.close();
				resourceBundles.put(
					bundleId,
					resourceBundle
				);
			}
			catch(Exception e) {
				try {
					java.net.URL bundleURL = servletContext.getResource("/wizards/ScheduleEventWizard_en_US.xml");
					InputStream in = bundleURL.openStream();
					resourceBundle = new Properties();
					resourceBundle.loadFromXML(in);
					in.close();
					resourceBundles.put(
						bundleId,
						resourceBundle
					);
				} catch(Exception e0) {}
			}
		}
		return resourceBundle;
	}

	public static String getDateAsString(
		GregorianCalendar date
	) {
		return getDateAsString(
			date.get(GregorianCalendar.YEAR),
			date.get(GregorianCalendar.MONTH) + 1,
			date.get(GregorianCalendar.DAY_OF_MONTH)
		);
	}

	public static String getDateAsString(
		int year,
		int month,
		int dayOfMonth
	) {
		return
			Integer.toString(year) +
			((month < 10 ? "0" : "") + Integer.toString(month)) +
			((dayOfMonth < 10 ? "0" : "") + Integer.toString(dayOfMonth));
	}

	public static String getSlotId(
		String dateAsString,
		int slotIndex
	) {
		return "calendar.slot." + dateAsString + "." + slotIndex;
	}

	public static String getVoteId(
	   int participantIndex,
	   String dateAsString,
	   int slotIndex
	) {
		return "vote." + participantIndex + "." + dateAsString + "." + slotIndex;
	}

	public static GregorianCalendar getDateAsCalendar(
		String dateAsString,
		ApplicationContext app

	) {
		GregorianCalendar date = new GregorianCalendar();
		date.setTimeZone(TimeZone.getTimeZone(app.getCurrentTimeZone()));
		date.setMinimalDaysInFirstWeek(4); // this conforms to DIN 1355/ISO 8601
		date.set(GregorianCalendar.YEAR, Integer.valueOf(dateAsString.substring(0, 4)));
		date.set(GregorianCalendar.MONTH, Integer.valueOf(dateAsString.substring(4, 6)) - 1);
		date.set(GregorianCalendar.DAY_OF_MONTH, Integer.valueOf(dateAsString.substring(6, 8)));
		return date;
	}

	// Parse slot value and return as event
	public static String parseSlotValue(
		String s,
		String dateAsString,
		SimpleDateFormat dateTimeParser24,
		SimpleDateFormat dateTimeParserAmPm,
		ApplicationContext app
	) {
    boolean timeMissing = false;
		if(s == null) return null;
    s = s.trim();
    if(s.length() == 0) return null;
		String[] n = new String[3];
		if(s.indexOf("-") > 0) {
			n[0] = s.substring(0, s.indexOf("-"));
			s = s.substring(s.indexOf("-") + 1);
			if(s.indexOf("@") > 0) {
				n[1] = s.substring(0, s.indexOf("@"));
				n[2] = s.substring(s.indexOf("@") + 1);
			}
			else {
				n[1] = s;
				n[2] = null;
			}
		}
		else {
			n[1] = null;
			if(s.indexOf("@") > 0) {
				n[0] = s.substring(0, s.indexOf("@"));
				n[2] = s.substring(s.indexOf("@") + 1);
			}
			else {
				n[0] = s;
				n[2] = null;
			}
		}
		// Normalize dateTimeFrom = e[0]
		String t = "";
    boolean startsWithDigit = false;
		for(int i = 0; i < n[0].length(); i++) {
			if(Character.isDigit(n[0].charAt(i))) {
				t += n[0].charAt(i);
        if (i==0) {startsWithDigit = true;}
			}
		}
		if(t.length() == 0 || !startsWithDigit) {
			t = "00:00";
      timeMissing = true;
		}
		else if(t.length() == 1) {
			t = "0" + t + ":00";
		}
		else if(t.length() == 2) {
			t = t + ":00";
		}
		else if(t.length() == 3) {
			t = "0" + t.charAt(0) + ":" + t.substring(1);
		}
		else {
			t = t.substring(0,2) + ":" + t.substring(2);
		}
		if(n[0].indexOf("am") > 0 || n[0].indexOf("AM") > 0) {
			t += " AM";
		}
		else if(n[0].indexOf("pm") > 0 || n[0].indexOf("PM") > 0) {
			t += " PM";
		}
		n[0] = t;
		// Normalize dateTimeTo = e[1]
		if(n[1] != null) {
			t = "";
			for(int i = 0; i < n[1].length(); i++) {
				if(Character.isDigit(n[1].charAt(i))) {
					t += n[1].charAt(i);
				}
			}
			if(t.length() == 0) {
				t = "00:00";
			}
			else if(t.length() == 1) {
				t = "0" + t + ":00";
			}
			else if(t.length() == 2) {
				t = t + ":00";
			}
			else if(t.length() == 3) {
				t = "0" + t.charAt(0) + ":" + t.substring(1);
			}
			else {
				t = t.substring(0,2) + ":" + t.substring(2);
			}
			if(n[1].indexOf("am") > 0 || n[1].indexOf("AM") > 0) {
				t += " AM";
			}
			else if(n[1].indexOf("pm") > 0 || n[1].indexOf("PM") > 0) {
				t += " PM";
			}
			n[1] = t;
		}
    if (timeMissing) {
        n[2] = "<" + s;
    }
		// Return event
		Date dateFrom = new Date();
		try {
			dateFrom = n[0].indexOf("AM") > 0 || n[0].indexOf("PM") > 0 ?
				dateTimeParserAmPm.parse(n[0] + " " + dateAsString) :
				dateTimeParser24.parse(n[0] + " " + dateAsString);
		} catch(Exception e) {}
		Date dateTo = new Date();
		try {
			dateTo = n[1] == null ?
				new Date(dateFrom.getTime() /* + 3600000L */) :
				n[1].indexOf("AM") > 0 || n[1].indexOf("PM") > 0 ?
					dateTimeParserAmPm.parse(n[1] + " " + dateAsString) :
					dateTimeParser24.parse(n[1] + " " + dateAsString);
			// If dateTo < dateFrom go to next day
			if(dateTo.compareTo(dateFrom) < 0) {
				GregorianCalendar date = getDateAsCalendar(dateAsString, app);
				date.add(GregorianCalendar.DAY_OF_MONTH, 1);
				String datePlusOneDayAsString = getDateAsString(date);
				dateTo = n[1] == null ?
					new Date(dateFrom.getTime() + 3600000L) :
					n[1].indexOf("AM") > 0 || n[1].indexOf("PM") > 0 ?
						dateTimeParserAmPm.parse(n[1] + " " + datePlusOneDayAsString) :
						dateTimeParser24.parse(n[1] + " " + datePlusOneDayAsString);
			}
		} catch(Exception e) {}
		org.w3c.format.DateTimeFormat standardDateFormat = org.w3c.format.DateTimeFormat.BASIC_UTC_FORMAT;
		return standardDateFormat.format(dateFrom) + (dateFrom.compareTo(dateTo) == 0 ? "" : "-" + standardDateFormat.format(dateTo)) + (n[2] == null ? "" : "@" + n[2]);
	}

	// Slot value is of the form datetimeFrom@location or datetimeFrom-datetimeTo@location
	public static String formatEvent(
		String event,
		SimpleDateFormat timeFormat
	) {
    if(event == null) return "";
    if(event.indexOf(TIME_MISSING)>0) return event.substring(22);
    if (event.length() < 20) return "";
		Date dateFrom = org.w3c.spi2.Datatypes.create(Date.class, event.substring(0, 20));
		Date dateTo = org.w3c.spi2.Datatypes.create(Date.class, event.length() < 41 ? event.substring(0, 20) : event.substring(21, 41));
		return timeFormat.format(dateFrom) + (dateFrom.compareTo(dateTo) == 0 ? "" : "-" + timeFormat.format(dateTo)) + (event.length() < 41 ? event.substring(20) : event.substring(41));
	}
  
  public static boolean isEventWithoutTime(
    String event
  ) {
    return ((event == null) || (event.indexOf(TIME_MISSING)>0));
  } 

	public static void disableActivities(
	   javax.jdo.PersistenceManager pm,
	   org.opencrx.kernel.activity1.jmi1.ActivityTracker tracker,
	   Date from,
	   Date to,
	   String correlation
	) {
		org.opencrx.kernel.activity1.cci2.ActivityQuery query =
			(org.opencrx.kernel.activity1.cci2.ActivityQuery)pm.newQuery(org.opencrx.kernel.activity1.jmi1.Activity.class);
		query.thereExistsScheduledStart().greaterThanOrEqualTo(from);
		query.thereExistsScheduledEnd().lessThanOrEqualTo(to);
		query.thereExistsExternalLink().equalTo(correlation);
		query.forAllDisabled().isFalse();
		pm.currentTransaction().begin();
		for(Iterator i = tracker.getFilteredActivity(query).iterator(); i.hasNext(); ) {
			org.opencrx.kernel.activity1.jmi1.Activity activity = (org.opencrx.kernel.activity1.jmi1.Activity)i.next();
			activity.setDisabled(Boolean.TRUE);
		}
		try {
			pm.currentTransaction().commit();
		}
		catch(Exception e) {
			new org.openmdx.base.exception.ServiceException(e).log();
			try {
				pm.currentTransaction().rollback();
			} catch(Exception e1) {}
		}
	}

	public static String getUsername(
		javax.jdo.PersistenceManager pm,
		org.opencrx.kernel.home1.jmi1.Segment homeSegment,
		String emailAddress
	) {
		//org.opencrx.kernel.home1.cci2.UserHomeQuery userHomeFilter = org.opencrx.kernel.utils.Utils.getHomePackage(pm).createUserHomeQuery();
		String userName = null;
		for(
			Iterator i = homeSegment.getUserHome().iterator();
			i.hasNext() && userName == null;
		) {
			org.opencrx.kernel.home1.jmi1.UserHome userHome = (org.opencrx.kernel.home1.jmi1.UserHome)i.next();
			if (userHome.getContact() != null) {
				// check whether any of the EMailAddresses match
				org.opencrx.kernel.account1.cci2.EMailAddressQuery emailAddressFilter = (org.opencrx.kernel.account1.cci2.EMailAddressQuery)pm.newQuery(org.opencrx.kernel.account1.jmi1.EMailAddress.class);
				emailAddressFilter.forAllDisabled().isFalse();
				for (
					Iterator j = userHome.getContact().getAddress(emailAddressFilter).iterator();
					j.hasNext() && userName == null;
				) {
          if (((org.opencrx.kernel.account1.jmi1.EMailAddress)j.next()).getEmailAddress().equalsIgnoreCase(emailAddress)) {
						userName = ((new Path(userHome.refMofId())).getLastSegment()).toString();
					}
				}
			}
		}
		return userName;
	}
%>
<%
	request.setCharacterEncoding("UTF-8");
	String servletPath = "." + request.getServletPath();
	String servletPathPrefix = servletPath.substring(0, servletPath.lastIndexOf("/") + 1);
	ApplicationContext app = (ApplicationContext)session.getAttribute(WebKeys.APPLICATION_KEY);
	ViewsCache viewsCache = (ViewsCache)session.getAttribute(WebKeys.VIEW_CACHE_KEY_SHOW);
	String requestId =  request.getParameter(Action.PARAMETER_REQUEST_ID);
	String objectXri = request.getParameter(Action.PARAMETER_OBJECTXRI);
	if(objectXri == null || app == null || viewsCache.getView(requestId) == null) {
		response.sendRedirect(
			request.getContextPath() + "/" + WebKeys.SERVLET_NAME
		);
		return;
	}
	Properties bundle = getResourceBundle(
		app.getCurrentLocale(),
		this.getServletContext()
	);
	javax.jdo.PersistenceManager pm = app.getNewPmData();
	RefObject_1_0 obj = (RefObject_1_0)pm.getObjectById(new Path(objectXri));
	Texts_1_0 texts = app.getTexts();
	Codes codes = app.getCodes();
	// Format times
	TimeZone timezone = TimeZone.getTimeZone(app.getCurrentTimeZone());
	SimpleDateFormat timeFormat = new SimpleDateFormat("HH:mm", app.getCurrentLocale());
	timeFormat.setTimeZone(timezone);
	SimpleDateFormat localizedDateTimeFormat = org.openmdx.portal.servlet.attribute.DateValue.getLocalizedDateTimeFormatter(null, false, app);
	// Parse date/times
	SimpleDateFormat dateTimeParser24 = new SimpleDateFormat("HH:mm yyyyMMdd", app.getCurrentLocale());
	dateTimeParser24.setTimeZone(timezone);
	SimpleDateFormat dateTimeParserAmPm = new SimpleDateFormat("h:mm a yyyyMMdd", app.getCurrentLocale());
	dateTimeParserAmPm.setTimeZone(timezone);

	// Get parameters
	String command = request.getParameter("Command");
	if(command == null) command = "";
	boolean actionRefresh = "Refresh".equals(command);
	boolean actionRefreshCal = "RefreshCal".equals(command);
	boolean actionCancel = "Cancel".equals(command);
	boolean actionNextMonth = "NextMonth".equals(command);
	boolean actionPrevMonth = "PrevMonth".equals(command);
	boolean actionNextYear = "NextYear".equals(command);
 	boolean actionPrevYear = "PrevYear".equals(command);
	boolean actionCopyFirstRow = "CopyFirstRow".equals(command);
	boolean actionAddVoter = "AddVoter".equals(command);
	boolean actionApply = "Apply".equals(command);
	boolean actionApplyCal = "ApplyCal".equals(command);
	boolean actionSave = "Save".equals(command);
 	String anchor = null;
	int voterCount = request.getParameter("voter.count") == null ?
		0 :
		Integer.valueOf(request.getParameter("voter.count"));
	if(actionRefreshCal || actionApplyCal) {
	  app.resetPmData();
	  anchor = ANCHOR_CALENDAR;
	}
	if(actionRefresh) {
	  app.resetPmData();
	  anchor = ANCHOR_VOTES;
	}
	if(actionApply) {
	  app.resetPmData();
	  anchor = ANCHOR_VOTES;
	}
	// Delete voter
	boolean actionDeleteVoter = command.startsWith("DeleteVoter.");
	int deleteVoterIndex = -1;
	if(actionDeleteVoter) {
		anchor = ANCHOR_PARTICIPANTS;
		deleteVoterIndex = Integer.valueOf(command.substring(command.indexOf(".") + 1));
	}
	// Notify voter
	boolean actionNotifyVoter = command.startsWith("NotifyVoter.");
	boolean actionNotifyVoterCompleted = false;
	int notifyVoterIndex = -1;
	String notifyVoterUsername = null;
	if(actionNotifyVoter) {
		anchor = ANCHOR_PARTICIPANTS;
		notifyVoterUsername = command.substring(command.indexOf(".") + 1);
		notifyVoterIndex = Integer.valueOf(notifyVoterUsername.substring(0, notifyVoterUsername.indexOf(".")));
		notifyVoterUsername = notifyVoterUsername.substring(notifyVoterUsername.indexOf(".") + 1);
	}

	// Add date
	boolean actionAddDate = command.startsWith("AddDate.");
	int dayOfMonth = -1;
	if(actionAddDate) {
		anchor = ANCHOR_CALENDAR;
		dayOfMonth = Integer.valueOf(command.substring(command.indexOf(".") + 1));
	}
	// Delete date
	boolean actionDeleteDate = command.startsWith("DeleteDate.");
	int dateIndex = -1;
	if(actionDeleteDate) {
		anchor = ANCHOR_CALENDAR;
		dateIndex = Integer.valueOf(command.substring(command.indexOf(".") + 1));
	}
	boolean actionCreateTentativeEvent = command.startsWith("CreateTentativeEvent.");
	String newEvent = null;
	if(actionCreateTentativeEvent) {
		anchor = ANCHOR_CALENDAR;
		newEvent = command.substring(command.indexOf(".") + 1);
	}
	boolean actionDisableTentativeEvent = command.startsWith("DisableTentativeEvent.");
	String disableEvent = null;
	if(actionDisableTentativeEvent) {
		anchor = ANCHOR_CALENDAR;
		disableEvent = command.substring(command.indexOf(".") + 1);
	}
	boolean actionCreateConfirmedEvent = command.startsWith("CreateConfirmedEvent.");
	if(actionCreateConfirmedEvent) {
		anchor = ANCHOR_VOTES;
		newEvent = command.substring(command.indexOf(".") + 1);
	}
	// Cancel
	if(actionCancel) {
	  session.setAttribute(WIZARD_NAME, null);
		Action nextAction = new ObjectReference(obj, app).getSelectObjectAction();
		response.sendRedirect(
			request.getContextPath() + "/" + nextAction.getEncodedHRef()
		);
		return;
	}
	// Schedule event form
	org.openmdx.ui1.jmi1.FormDefinition formDefinition = app.getUiFormDefinition(FORM_NAME);
	org.openmdx.portal.servlet.control.FormControl form = new org.openmdx.portal.servlet.control.FormControl(
		formDefinition.refGetPath().getLastSegment().toString(),
		app.getCurrentLocaleAsString(),
		app.getCurrentLocaleAsIndex(),
		app.getUiContext(),
		formDefinition
	);
	// Participant form
	org.openmdx.ui1.jmi1.FormDefinition addVoterFormDefinition = app.getUiFormDefinition("AddVoterForm");
	org.openmdx.portal.servlet.control.FormControl addVoterForm = new org.openmdx.portal.servlet.control.FormControl(
		addVoterFormDefinition.refGetPath().getLastSegment().toString(),
		app.getCurrentLocaleAsString(),
		app.getCurrentLocaleAsIndex(),
		app.getUiContext(),
		addVoterFormDefinition
	);
	Map formValues = new HashMap();
	// If wizard is launched from EMail activity map activity to formValues.
	// Otherwise get parameters from submitted form
	formValues.put(
		"Mode",
		request.getParameter("Mode")
	);
	List selectedDates = new ArrayList();
	if(
		(request.getParameter("Mode") == null) &&
		(obj instanceof org.opencrx.kernel.activity1.jmi1.EMail)
	) {
		org.opencrx.kernel.activity1.jmi1.EMail email = (org.opencrx.kernel.activity1.jmi1.EMail)obj;
		formValues.put(
			"emailCreator",
			email.getLastAppliedCreator().refGetPath()
		);
		formValues.put(
			"org:opencrx:kernel:activity1:Activity:name",
			email.getMessageSubject()
		);
		formValues.put(
			"org:opencrx:kernel:activity1:Activity:description",
			email.getDescription()
		);
		formValues.put(
			"org:opencrx:kernel:activity1:Activity:detailedDescription",
			email.getDetailedDescription()
		);
		formValues.put(
			"messageBody",
			email.getMessageBody()
		);
		// Voters
		int ii = 0;
		for(Iterator i = email.getEmailRecipient().iterator(); i.hasNext(); ) {
			org.opencrx.kernel.activity1.jmi1.AbstractEMailRecipient recipient = (org.opencrx.kernel.activity1.jmi1.AbstractEMailRecipient)i.next();
			if(recipient instanceof org.opencrx.kernel.activity1.jmi1.EMailRecipient) {
				if(((org.opencrx.kernel.activity1.jmi1.EMailRecipient)recipient).getParty() instanceof org.opencrx.kernel.account1.jmi1.EMailAddress) {
					formValues.put(
						"voter." + ii,
						((org.opencrx.kernel.activity1.jmi1.EMailRecipient)recipient).getParty().refGetPath()
					);
					ii++;
				}
			}
		}
		voterCount = ii;
		formValues.put(
			"voter.count",
			voterCount
		);
		// Get event and poll info
		formValues.put(
			"isClosedGroupPoll",
			Boolean.FALSE
		);
		formValues.put(
			"isHiddenPoll",
			Boolean.FALSE
		);
		formValues.put(
  		"isYesNoPoll",
  		Boolean.FALSE
  	);
		formValues.put(
  		"isLimitTo1Poll",
  		Boolean.FALSE
  	);
		for(Iterator i = email.getNote().iterator(); i.hasNext(); ) {
			org.opencrx.kernel.generic.jmi1.Note note = (org.opencrx.kernel.generic.jmi1.Note)i.next();
			if(WIZARD_NAME.equals(note.getTitle())) {
				Properties eventAndPollInfo = new Properties();
				ByteArrayInputStream in = new ByteArrayInputStream(note.getText().getBytes("UTF-8"));
				eventAndPollInfo.loadFromXML(in);
				in.close();
				formValues.put(
					"isClosedGroupPoll",
					Boolean.valueOf(eventAndPollInfo.getProperty("isClosedGroupPoll", "false"))
				);
				formValues.put(
					"isHiddenPoll",
					Boolean.valueOf(eventAndPollInfo.getProperty("isHiddenPoll", "false"))
				);
				formValues.put(
					"isYesNoPoll",
					Boolean.valueOf(eventAndPollInfo.getProperty("isYesNoPoll", "false"))
				);
				formValues.put(
						"isLimitTo1Poll",
						Boolean.valueOf(eventAndPollInfo.getProperty("isLimitTo1Poll", "false"))
					);
				if(eventAndPollInfo.containsKey("eventTracker")) {
					try {
						formValues.put(
							"eventTracker",
							new Path(eventAndPollInfo.getProperty("eventTracker"))
						);
					} catch(Exception e) {}
				}
				// Get sorted events
				Map slots = new TreeMap();
				Enumeration propertyNames = eventAndPollInfo.propertyNames();
				GregorianCalendar date = new GregorianCalendar();
				date.setTimeZone(TimeZone.getTimeZone(app.getCurrentTimeZone()));
				date.setMinimalDaysInFirstWeek(4); // this conforms to DIN 1355/ISO 8601
				date.setTimeZone(timezone);
				org.w3c.format.DateTimeFormat dateFormat = org.w3c.format.DateTimeFormat.BASIC_UTC_FORMAT;
				while(propertyNames.hasMoreElements()) {
					String propertyName = (String)propertyNames.nextElement();
					if(propertyName.startsWith("event.")) {
					  String event = eventAndPollInfo.getProperty(propertyName);
						date.setTime(dateFormat.parse(event.substring(0, 20)));
						String dateAsString = getDateAsString(date);
						if(slots.get(dateAsString) == null) {
							slots.put(
								dateAsString,
								new TreeSet()
							);
						}
						((Set)slots.get(dateAsString)).add(event);
					}
				}
				selectedDates = new ArrayList(slots.keySet());
				formValues.put(
				  "calendar.selectedDates",
				  selectedDates
				);
				for(Iterator j = selectedDates.iterator(); j.hasNext(); ) {
					String dateAsString = (String)j.next();
					int kk = 0;
					for(Iterator k = ((Set)slots.get(dateAsString)).iterator(); k.hasNext(); kk++) {
						String event = (String)k.next();
						formValues.put(
							getSlotId(dateAsString, kk),
							event
						);
					}
				}
			}
		}
		formValues.put(
			"Mode",
			"edit"
		);
	}
	else {
		form.updateObject(
			request.getParameterMap(),
			formValues,
			app,
			pm
		);
		addVoterForm.updateObject(
			request.getParameterMap(),
			formValues,
			app,
			pm
		);
		// Email activity creator
		if(request.getParameter("emailCreator") != null) {
			formValues.put(
				"emailCreator",
				new Path(request.getParameter("emailCreator"))
			);
		}
		// Event activity creator
		if(request.getParameter("eventTracker") != null) {
			String xri = request.getParameter("eventTracker");
			if(xri.startsWith("xri:")) {
				formValues.put(
					"eventTracker",
					new Path(xri)
				);
			}
		}
		if(request.getParameter("messageBody") != null) {
			formValues.put(
				"messageBody",
				request.getParameter("messageBody")
			);
		}
		formValues.put(
			"isClosedGroupPoll",
			"on".equals(request.getParameter("isClosedGroupPoll"))
		);
		formValues.put(
			"isHiddenPoll",
			"on".equals(request.getParameter("isHiddenPoll"))
		);
		formValues.put(
			"isYesNoPoll",
			"on".equals(request.getParameter("isYesNoPoll"))
		);
		formValues.put(
			"isLimitTo1Poll",
			"on".equals(request.getParameter("isLimitTo1Poll"))
		);
		// Get calendar
		formValues.put(
			"calendar.month",
			request.getParameter("calendar.month") == null ?
			   null :
			   Integer.valueOf(request.getParameter("calendar.month"))
		);
		formValues.put(
			"calendar.year",
			request.getParameter("calendar.year") == null ?
			   null :
			   Integer.valueOf(request.getParameter("calendar.year"))
		);
		Set dates = new TreeSet();
		for(int i = 0; i < 100; i++) {
			String selectedDateId = "calendar.selectedDate." + i;
			if(request.getParameter(selectedDateId) != null) {
				String dateAsString = request.getParameter(selectedDateId);
				dates.add(dateAsString);
				for(int j = 0; j < NUM_SLOTS; j++) {
					String slotId = getSlotId(dateAsString, j);
					formValues.put(
						slotId,
						parseSlotValue(
							request.getParameter(slotId),
							dateAsString,
							dateTimeParser24,
							dateTimeParserAmPm,
							app
						)
			  	);
				}
			}
		}
		selectedDates = new ArrayList(dates);
		formValues.put(
			"calendar.selectedDates",
			selectedDates
	 	);
		// Get voters
		for(int i = 0; i < 100; i++) {
			String addressXri = request.getParameter("voter." + i);
			if(addressXri != null) {
				formValues.put(
					"voter." + i,
					new Path(addressXri)
				);
			}
		}
	}

	String providerName = obj.refGetPath().get(2);
	String segmentName = obj.refGetPath().get(4);
	org.opencrx.kernel.activity1.jmi1.Segment activitySegment =
		(org.opencrx.kernel.activity1.jmi1.Segment)pm.getObjectById(
			new Path("xri:@openmdx:org.opencrx.kernel.activity1/provider/" + providerName + "/segment/" + segmentName)
		);
	org.opencrx.kernel.home1.jmi1.Segment homeSegment =
		(org.opencrx.kernel.home1.jmi1.Segment)pm.getObjectById(
			new Path("xri:@openmdx:org.opencrx.kernel.home1/provider/" + providerName + "/segment/" + segmentName)
		);
	TransientObjectView view = new TransientObjectView(
		formValues,
		app,
		obj,
		pm
	);
	ViewPort p = ViewPortFactory.openPage(
		view,
		request,
		out
	);
	p.setResourcePathPrefix("../../");

	boolean isEditMode = "edit".equals(formValues.get("Mode"));
	if(actionAddVoter) {
		anchor = ANCHOR_PARTICIPANTS;
		org.opencrx.kernel.account1.jmi1.AccountAddress party = formValues.get("org:opencrx:kernel:activity1:EMailRecipient:party") != null ?
			(org.opencrx.kernel.account1.jmi1.AccountAddress)pm.getObjectById(
				formValues.get("org:opencrx:kernel:activity1:EMailRecipient:party")
			) : null;
		if(
			(party != null) &&
			(party instanceof org.opencrx.kernel.account1.jmi1.EMailAddress)
		) {
			org.opencrx.kernel.account1.jmi1.Account account = null;
			try {
				account = (org.opencrx.kernel.account1.jmi1.Account)pm.getObjectById(party.refGetPath().getParent().getParent());
			} catch(Exception e) {}
			if(account != null) {
				boolean found = false;
				for(int i = 0; i < voterCount; i++) {
					if(
						(formValues.get("voter." + i) != null) &&
						party.equals(formValues.get("voter." + i))
					) {
						found = true;
					}
				}
				if(!found) {
					formValues.put(
						"voter." + voterCount,
						party.refGetPath()
					);
					voterCount++;
				}
			}
		}
	}
	if(actionDeleteVoter) {
		anchor = ANCHOR_PARTICIPANTS;
		formValues.remove("voter." + deleteVoterIndex);
	}
	if(actionNotifyVoter) {
		anchor = ANCHOR_PARTICIPANTS;
		if(notifyVoterUsername != null && formValues.get("voter." + notifyVoterIndex) != null) {
			String subject = formValues.get("org:opencrx:kernel:activity1:Activity:name") != null
			  ? (String)formValues.get("org:opencrx:kernel:activity1:Activity:name")
			  : "";
			String body = formValues.get("messageBody") != null
			  ? (String)formValues.get("messageBody")
			  : "";
			//System.out.println("notify: " + notifyVoterUsername);
			pm.currentTransaction().begin();
			org.openmdx.base.jmi1.BasicObject reference = null;
			org.opencrx.kernel.base.jmi1.SendAlertParams sendAlertParams = org.w3c.spi2.Structures.create(
				org.opencrx.kernel.base.jmi1.SendAlertParams.class, 
           		org.w3c.spi2.Datatypes.member(org.opencrx.kernel.base.jmi1.SendAlertParams.Member.description, body),
           		org.w3c.spi2.Datatypes.member(org.opencrx.kernel.base.jmi1.SendAlertParams.Member.importance, IMPORTANCE_HIGH),    		
           		org.w3c.spi2.Datatypes.member(org.opencrx.kernel.base.jmi1.SendAlertParams.Member.name, subject),    		
           		org.w3c.spi2.Datatypes.member(org.opencrx.kernel.base.jmi1.SendAlertParams.Member.resendDelayInSeconds, null),    		
           		org.w3c.spi2.Datatypes.member(org.opencrx.kernel.base.jmi1.SendAlertParams.Member.toUsers, notifyVoterUsername),	
           		org.w3c.spi2.Datatypes.member(org.opencrx.kernel.base.jmi1.SendAlertParams.Member.reference, reference)	
           	);
			// get current user's UserHome and send alert
			org.opencrx.kernel.home1.jmi1.UserHome currentUserHome = (org.opencrx.kernel.home1.jmi1.UserHome)pm.getObjectById(app.getUserHomeIdentityAsPath());
			currentUserHome.sendAlert(sendAlertParams);
			try {
				pm.currentTransaction().commit();
				actionNotifyVoterCompleted = true;
			}
			catch(Exception e) {
				new org.openmdx.base.exception.ServiceException(e).log();
				try {
					pm.currentTransaction().rollback();
				} catch(Exception e1) {}
			}
		}
	}
	if(actionPrevMonth) {
		anchor = ANCHOR_CALENDAR;
		Integer calendarMonth = (Integer)formValues.get("calendar.month");
		Integer calendarYear = (Integer)formValues.get("calendar.year");
		if(calendarMonth != null && calendarYear != null) {
			if(calendarMonth <= 0) {
				formValues.put(
					"calendar.year",
					calendarYear - 1
				);
				formValues.put(
					"calendar.month",
					11
				);
			}
			else {
				formValues.put(
					"calendar.month",
					calendarMonth - 1
				);
			}
		}
	}
	if(actionNextMonth) {
		anchor = ANCHOR_CALENDAR;
		Integer calendarMonth = (Integer)formValues.get("calendar.month");
		Integer calendarYear = (Integer)formValues.get("calendar.year");
		if(calendarMonth != null && calendarYear != null) {
			if(calendarMonth >= 11) {
				formValues.put(
					"calendar.year",
					calendarYear + 1
				);
				formValues.put(
					"calendar.month",
					0
				);
			}
			else {
				formValues.put(
					"calendar.month",
					calendarMonth + 1
				);
			}
		}
	}
	if(actionPrevYear) {
		anchor = ANCHOR_CALENDAR;
		Integer calendarYear = (Integer)formValues.get("calendar.year");
		if(calendarYear != null) {
			formValues.put(
				"calendar.year",
				calendarYear - 1
			);
		}
	}
	if(actionNextYear) {
		anchor = ANCHOR_CALENDAR;
		Integer calendarYear = (Integer)formValues.get("calendar.year");
		if(calendarYear != null) {
			formValues.put(
				"calendar.year",
				calendarYear + 1
			);
		}
	}
	if(actionAddDate) {
		anchor = ANCHOR_CALENDAR;
		Integer calendarMonth = (Integer)formValues.get("calendar.month");
		Integer calendarYear = (Integer)formValues.get("calendar.year");
		if(calendarMonth != null && calendarYear != null && dayOfMonth > 0) {
			selectedDates = (List)formValues.get("calendar.selectedDates");
			String dateAsString = getDateAsString(
				calendarYear,
				calendarMonth + 1,
				dayOfMonth
		   	);
			if(!selectedDates.contains(dateAsString)) {
				Set dates = new TreeSet(selectedDates);
				dates.add(dateAsString);
				selectedDates = new ArrayList(dates);
				for(int i = 0; i < NUM_SLOTS; i++) {
					String slotId = getSlotId(dateAsString, i);
					formValues.put(slotId, "");
				}
			}
		}
	}
	if(actionDeleteDate) {
		anchor = ANCHOR_CALENDAR;
		selectedDates = (List)formValues.get("calendar.selectedDates");
		if((dateIndex >= 0) && (dateIndex < selectedDates.size())) {
			String dateAsString = (String)selectedDates.remove(dateIndex);
			if(dateAsString != null) {
				for(int i = 0; i < NUM_SLOTS; i++) {
					String slotId = getSlotId(dateAsString, i);
					formValues.remove(slotId);
				}
				org.opencrx.kernel.activity1.jmi1.ActivityTracker eventTracker = formValues.get("eventTracker") != null ? 
					(org.opencrx.kernel.activity1.jmi1.ActivityTracker)pm.getObjectById(
						formValues.get("eventTracker")
					) : null;
				if(isEditMode && eventTracker != null) {
					org.w3c.format.DateTimeFormat dateFormat = org.w3c.format.DateTimeFormat.BASIC_UTC_FORMAT;
					Date from = dateFormat.parse(dateAsString + "T000000.000Z");
					Date to = dateFormat.parse(dateAsString + "T235959.999Z");
					disableActivities(
						pm,
						eventTracker,
						from,
						to,
						obj.refGetPath().getLastSegment().toString()
					);
				}
			}
		}
	}
	if(actionCopyFirstRow) {
		anchor = ANCHOR_CALENDAR;
		if(!selectedDates.isEmpty()) {
			String firstDateAsString = (String)selectedDates.get(0);
			for(int i = 1; i < selectedDates.size(); i++) {
				for(int j = 0; j < NUM_SLOTS; j++) {
					String dateAsString = (String)selectedDates.get(i);
					String slotIdFrom = getSlotId(firstDateAsString, j);
					String slotIdTo = getSlotId(dateAsString, j);
					if((formValues.get(slotIdTo) == null) || (((String)formValues.get(slotIdTo)).length() == 0)) {
						String event = (String)formValues.get(slotIdFrom);
						if(event != null) {
							event = event.replace(firstDateAsString, dateAsString);
						}
						formValues.put(
							slotIdTo,
							event
						);
					}
				}
			}
		}
	}
	if(actionCreateTentativeEvent || actionCreateConfirmedEvent) {
		anchor = ANCHOR_CALENDAR;
		org.opencrx.kernel.activity1.jmi1.ActivityTracker eventTracker = formValues.get("eventTracker") != null ?
			(org.opencrx.kernel.activity1.jmi1.ActivityTracker)pm.getObjectById(
				formValues.get("eventTracker")
			) : null;
		String name = (String)formValues.get("org:opencrx:kernel:activity1:Activity:name");
		String description = (String)formValues.get("org:opencrx:kernel:activity1:Activity:description");
		String detailedDescription = (String)formValues.get("org:opencrx:kernel:activity1:Activity:detailedDescription");
		if(
			(eventTracker != null) &&
			(newEvent != null) &&
			(eventTracker.getDefaultCreator() != null) &&
			(name != null)
		) {
			org.w3c.format.DateTimeFormat dateFormat = org.w3c.format.DateTimeFormat.BASIC_UTC_FORMAT;
			// In case of actionCreateConfirmedEvent disable all tentative events
			if(actionCreateConfirmedEvent) {
				Date from = dateFormat.parse(selectedDates.get(0) + "T000000.000Z");
				Date to = dateFormat.parse(selectedDates.get(selectedDates.size() - 1) + "T235959.999Z");
				disableActivities(
					pm,
					eventTracker,
					from,
					to,
					obj.refGetPath().getLastSegment().toString()
				);
			}
			// Create confirmed event
			Date scheduledStart = dateFormat.parse(newEvent.substring(0, 20));
			Date scheduledEnd = newEvent.length() < 41 ? dateFormat.parse(newEvent.substring(0, 20)) : dateFormat.parse(newEvent.substring(21, 41));
			String location = newEvent.length() < 41 ? newEvent.substring(20) : (newEvent.length() >= 42 ? newEvent.substring(42) : null);
			org.opencrx.kernel.activity1.jmi1.NewActivityParams params = org.w3c.spi2.Structures.create(
				org.opencrx.kernel.activity1.jmi1.NewActivityParams.class, 
   				org.w3c.spi2.Datatypes.member(org.opencrx.kernel.activity1.jmi1.NewActivityParams.Member.creationContext, null),
   				org.w3c.spi2.Datatypes.member(org.opencrx.kernel.activity1.jmi1.NewActivityParams.Member.description, description),
   				org.w3c.spi2.Datatypes.member(org.opencrx.kernel.activity1.jmi1.NewActivityParams.Member.detailedDescription, detailedDescription),
   				org.w3c.spi2.Datatypes.member(org.opencrx.kernel.activity1.jmi1.NewActivityParams.Member.dueBy, null),
   				org.w3c.spi2.Datatypes.member(org.opencrx.kernel.activity1.jmi1.NewActivityParams.Member.icalType, org.opencrx.kernel.backend.ICalendar.ICAL_TYPE_NA),
   				org.w3c.spi2.Datatypes.member(org.opencrx.kernel.activity1.jmi1.NewActivityParams.Member.name, name),
   				org.w3c.spi2.Datatypes.member(org.opencrx.kernel.activity1.jmi1.NewActivityParams.Member.priority, (short)0),
   				org.w3c.spi2.Datatypes.member(org.opencrx.kernel.activity1.jmi1.NewActivityParams.Member.reportingContact, null),
   				org.w3c.spi2.Datatypes.member(org.opencrx.kernel.activity1.jmi1.NewActivityParams.Member.scheduledEnd, scheduledEnd),
   				org.w3c.spi2.Datatypes.member(org.opencrx.kernel.activity1.jmi1.NewActivityParams.Member.scheduledStart, scheduledStart)
   			); 
			pm.currentTransaction().begin();
			org.opencrx.kernel.activity1.jmi1.NewActivityResult result = eventTracker.getDefaultCreator().newActivity(params);
			try {
				pm.currentTransaction().commit();
				org.opencrx.kernel.activity1.jmi1.Activity activity = (org.opencrx.kernel.activity1.jmi1.Activity )pm.getObjectById(new Path(result.getActivity().refMofId()));
				pm.currentTransaction().begin();
				activity.setLocation(location);
				// Link to activity which created the event
				activity.getExternalLink().add(obj.refGetPath().getLastSegment().toString());
				try {
					pm.currentTransaction().commit();
				}
				catch(Exception e) {
					new org.openmdx.base.exception.ServiceException(e).log();
					try {
						pm.currentTransaction().rollback();
					} catch(Exception e1) {}
				}
				pm.currentTransaction().begin();
				org.opencrx.kernel.activity1.jmi1.ActivityLinkTo activityLinkTo = pm.newInstance(org.opencrx.kernel.activity1.jmi1.ActivityLinkTo.class);
				activityLinkTo.setLinkTo((org.opencrx.kernel.activity1.jmi1.Activity)obj);
				activityLinkTo.setName("TEST");
				activityLinkTo.setActivityLinkType(new Short(CODE_ACTIVITYLINKTYPE_RELATESTO)); // relates to
				activity.addActivityLinkTo(
					org.opencrx.kernel.backend.Base.getInstance().getUidAsString(),
					activityLinkTo
				);
				try {
					pm.currentTransaction().commit();
				}
				catch(Exception e) {
					new org.openmdx.base.exception.ServiceException(e).log();
					try {
						pm.currentTransaction().rollback();
					} catch(Exception e1) {}
				}


			}
			catch(Exception e) {
				new org.openmdx.base.exception.ServiceException(e).log();
				try {
					pm.currentTransaction().rollback();
				} catch(Exception e1) {}
			}
			app.resetPmData();
		}
	}
	if(actionDisableTentativeEvent) {
		org.opencrx.kernel.activity1.jmi1.ActivityTracker eventTracker = formValues.get("eventTracker") != null ?
			(org.opencrx.kernel.activity1.jmi1.ActivityTracker)pm.getObjectById(
				formValues.get("eventTracker")
			) : null;
		if(isEditMode && eventTracker != null) {
			org.w3c.format.DateTimeFormat dateFormat = org.w3c.format.DateTimeFormat.BASIC_UTC_FORMAT;
			Date scheduledStart = dateFormat.parse(disableEvent.substring(0, 20));
			Date scheduledEnd = disableEvent.length() < 41 ? dateFormat.parse(disableEvent.substring(0, 20)) : dateFormat.parse(disableEvent.substring(21, 41));

			disableActivities(
				pm,
				eventTracker,
				scheduledStart,
				scheduledEnd,
				obj.refGetPath().getLastSegment().toString()
			);
		}
	}
	if(actionSave || actionApply || actionApplyCal) {
		String name = (String)formValues.get("org:opencrx:kernel:activity1:Activity:name");
		String description = (String)formValues.get("org:opencrx:kernel:activity1:Activity:description");
		String detailedDescription = (String)formValues.get("org:opencrx:kernel:activity1:Activity:detailedDescription");
		String messageBody = (String)formValues.get("messageBody");
		org.opencrx.kernel.activity1.jmi1.ActivityCreator emailCreator = formValues.get("emailCreator") != null ? 
			(org.opencrx.kernel.activity1.jmi1.ActivityCreator)pm.getObjectById(
				formValues.get("emailCreator")
			) : null;
		if(
			(name != null) &&
			(name.trim().length() > 0) &&
			(isEditMode || emailCreator != null)
		) {
			org.opencrx.kernel.activity1.jmi1.EMail activity = null;
			if(isEditMode) {
				activity = (org.opencrx.kernel.activity1.jmi1.EMail)obj;
			}
			else {
				// Create EMail activity
				org.opencrx.kernel.activity1.jmi1.NewActivityParams params = org.w3c.spi2.Structures.create(
					org.opencrx.kernel.activity1.jmi1.NewActivityParams.class, 
	   				org.w3c.spi2.Datatypes.member(org.opencrx.kernel.activity1.jmi1.NewActivityParams.Member.creationContext, null),
	   				org.w3c.spi2.Datatypes.member(org.opencrx.kernel.activity1.jmi1.NewActivityParams.Member.description, description),
	   				org.w3c.spi2.Datatypes.member(org.opencrx.kernel.activity1.jmi1.NewActivityParams.Member.detailedDescription, null),
	   				org.w3c.spi2.Datatypes.member(org.opencrx.kernel.activity1.jmi1.NewActivityParams.Member.dueBy, null),
	   				org.w3c.spi2.Datatypes.member(org.opencrx.kernel.activity1.jmi1.NewActivityParams.Member.icalType, org.opencrx.kernel.backend.ICalendar.ICAL_TYPE_NA),
	   				org.w3c.spi2.Datatypes.member(org.opencrx.kernel.activity1.jmi1.NewActivityParams.Member.name, name),
	   				org.w3c.spi2.Datatypes.member(org.opencrx.kernel.activity1.jmi1.NewActivityParams.Member.priority, (short)0),
	   				org.w3c.spi2.Datatypes.member(org.opencrx.kernel.activity1.jmi1.NewActivityParams.Member.reportingContact, null),
	   				org.w3c.spi2.Datatypes.member(org.opencrx.kernel.activity1.jmi1.NewActivityParams.Member.scheduledEnd, null),
	   				org.w3c.spi2.Datatypes.member(org.opencrx.kernel.activity1.jmi1.NewActivityParams.Member.scheduledStart, null)
	   			); 
				pm.currentTransaction().begin();
				org.opencrx.kernel.activity1.jmi1.NewActivityResult result = null;
        try {
	  			result = emailCreator.newActivity(params);
					pm.currentTransaction().commit();
				}
				catch(Exception e) {
					new org.openmdx.base.exception.ServiceException(e).log();
					try {
						pm.currentTransaction().rollback();
					} catch(Exception e1) {}
				}
        if (result != null) {
		  	activity = (org.opencrx.kernel.activity1.jmi1.EMail)pm.getObjectById(new Path(result.getActivity().refMofId()));
		  	objectXri = activity.refMofId();
        }
		isEditMode = false;
		formValues.put(
			"Mode",
			"edit"
		);
  	  } 
      if (activity != null) {
    			// Update EMail activity
    			org.opencrx.kernel.home1.jmi1.UserHome currentUserHome = (org.opencrx.kernel.home1.jmi1.UserHome)pm.getObjectById(app.getUserHomeIdentityAsPath());
    			pm.currentTransaction().begin();
    			activity.setName(name);
    			activity.setDescription(description);
    			activity.setDetailedDescription(detailedDescription);
    			activity.setMessageSubject(name);
    			if((messageBody == null) || (messageBody.indexOf(activity.refGetPath().getLastSegment().toString()) < 0)) {
    				String participationLink = currentUserHome.getWebAccessUrl() + "/" + VOTE_FOR_EVENT_WIZARD_NAME + "?id=" + providerName + "/" + segmentName + "/" + activity.refGetPath().getLastSegment().toString();
    				if(messageBody == null) messageBody = "";
    				messageBody += "\n\nParticipation link:\n" +  participationLink;
    			}
    			formValues.put(
    				"messageBody",
    				messageBody
    			);
    			activity.setMessageBody(messageBody);
    			if(!isEditMode) {
    				org.opencrx.kernel.account1.jmi1.AccountAddress[] userMainAddresses = org.opencrx.kernel.backend.Accounts.getInstance().getMainAddresses(currentUserHome.getContact());
    				activity.setSender(
    					userMainAddresses[org.opencrx.kernel.backend.Accounts.MAIL_BUSINESS] == null ?
    						userMainAddresses[org.opencrx.kernel.backend.Accounts.MAIL_HOME] :
    						userMainAddresses[org.opencrx.kernel.backend.Accounts.MAIL_BUSINESS]
    				);
    			}
    			// Remove recipients
    			for(Iterator i = activity.getEmailRecipient().iterator(); i.hasNext(); ) {
    				org.opencrx.kernel.activity1.jmi1.AbstractEMailRecipient r = (org.opencrx.kernel.activity1.jmi1.AbstractEMailRecipient)i.next();
    				if(r instanceof org.opencrx.kernel.activity1.jmi1.EMailRecipient) {
	    				boolean toBeRemoved = true;
	    				for(int j = 0; j < voterCount; j++) {
	    					org.opencrx.kernel.account1.jmi1.AccountAddress address = formValues.get("voter." + j) != null ? 
	    						(org.opencrx.kernel.account1.jmi1.AccountAddress)pm.getObjectById(
	    							formValues.get("voter." + j)
	    						) : null;
	    					if(address.equals(((org.opencrx.kernel.activity1.jmi1.EMailRecipient)r).getParty())) {
	    						toBeRemoved = false;
	    						break;
	    					}
	    				}
	    				if(toBeRemoved) {
	    					r.refDelete();
	    				}
    				}
    			}
    			// Add recipients
    			for(int i = 0; i < voterCount; i++) {
    				org.opencrx.kernel.account1.jmi1.AccountAddress address = formValues.get("voter." + i) != null ? 
    					(org.opencrx.kernel.account1.jmi1.AccountAddress)pm.getObjectById(
    						formValues.get("voter." + i)
    					) : null;
    				org.opencrx.kernel.activity1.jmi1.EMailRecipient recipient = null;
    				for(Iterator j = activity.getEmailRecipient().iterator(); j.hasNext(); ) {
    					org.opencrx.kernel.activity1.jmi1.AbstractEMailRecipient r = (org.opencrx.kernel.activity1.jmi1.AbstractEMailRecipient)j.next();
    					if(r instanceof org.opencrx.kernel.activity1.jmi1.EMailRecipient) {
	    					if(address.equals(((org.opencrx.kernel.activity1.jmi1.EMailRecipient)r).getParty())) {
	    						recipient = (org.opencrx.kernel.activity1.jmi1.EMailRecipient)r;
	    						break;
	    					}
    					}
    				}
    				if(recipient == null) {
    					recipient = pm.newInstance(org.opencrx.kernel.activity1.jmi1.EMailRecipient.class);
    					recipient.setPartyType(org.opencrx.kernel.backend.Activities.PartyType.EMAIL_TO.getValue());
    					recipient.setParty(
    						formValues.get("voter." + i) != null ?
	    						(org.opencrx.kernel.account1.jmi1.AccountAddress)pm.getObjectById(
	    							formValues.get("voter." + i)
	    						) : null
    					);
    					activity.addEmailRecipient(
    						false,
    						org.opencrx.kernel.backend.Base.getInstance().getUidAsString(),
    						recipient
    					);
    				}
    			}
    			// Add note containing events listed in schedule
    			Properties eventAndPollInfo = new Properties();
    			eventAndPollInfo.put(
       				"isClosedGroupPoll",
       				formValues.get("isClosedGroupPoll").toString()
       			);
    			eventAndPollInfo.put(
       				"isHiddenPoll",
       			 	formValues.get("isHiddenPoll").toString()
       			);
    			eventAndPollInfo.put(
       				"isYesNoPoll",
       			 	formValues.get("isYesNoPoll").toString()
       			);
    			eventAndPollInfo.put(
       				"isLimitTo1Poll",
       			 	formValues.get("isLimitTo1Poll").toString()
       			);
    			if(formValues.get("eventTracker") != null) {
    				eventAndPollInfo.put(
    	   				"eventTracker",
    	   				((Path)formValues.get("eventTracker")).toXRI()
    	   			);
    			}
    			int eventIndex = 0;
    			for(int i = 0; i < selectedDates.size(); i++) {
    				String dateAsString = (String)selectedDates.get(i);
    				for(int j = 0; j < NUM_SLOTS; j++) {
    					String slotId = getSlotId(dateAsString, j);
    					String event = (String)formValues.get(slotId);
    					if(event != null) {
    						eventAndPollInfo.setProperty(
    							"event." + eventIndex++,
    							event
    						);
    					}
    				}
    			}
    			org.opencrx.kernel.generic.jmi1.Note note = null;
    			for(Iterator i = activity.getNote().iterator(); i.hasNext(); ) {
    				org.opencrx.kernel.generic.jmi1.Note n = (org.opencrx.kernel.generic.jmi1.Note)i.next();
    				if(WIZARD_NAME.equals(n.getTitle())) {
    					note = n;
    					break;
    				}
    			}
    			if(note == null) {
    				note = pm.newInstance(org.opencrx.kernel.generic.jmi1.Note.class);
    				activity.addNote(
    					false,
    					org.opencrx.kernel.backend.Base.getInstance().getUidAsString(),
    					note
    				);
    			}
    			note.setTitle(WIZARD_NAME);
    			ByteArrayOutputStream bos = new ByteArrayOutputStream();
    			eventAndPollInfo.storeToXML(
    				bos,
    				WIZARD_NAME,
    				"UTF-8"
    			);
    			bos.close();
    			note.setText(new String(bos.toByteArray(), "UTF-8"));
    			try {
    				pm.currentTransaction().commit();
    				if(actionSave) {
    					// Forward request
    					session.setAttribute(WIZARD_NAME, null);
    					Action nextAction = new ObjectReference(
    						activity,
    						app
    				   	).getSelectObjectAction();
    					response.sendRedirect(
    						request.getContextPath() + "/" + nextAction.getEncodedHRef()
    					);
    					return;
    				}
    			}
    			catch(Exception e) {
    				new org.openmdx.base.exception.ServiceException(e).log();
    				try {
    					pm.currentTransaction().rollback();
    				} catch(Exception e1) {}
    			}
      }
		}
	}
	int tabIndex = 1;
%>
<!DOCTYPE HTML PUBLIC "-//W3C//DTD HTML 4.01 Transitional//EN" "http://www.w3.org/TR/html4/loose.dtd">
<html dir="<%= texts.getDir() %>">
<head>
	<title>openCRX - Schedule Event</title>
	<link rel="stylesheet" href="../../js/bootstrap/css/bootstrap.min.css">	
	<link rel="stylesheet" href="../../_style/colors.css">
	<link rel="stylesheet" href="../../_style/n2default.css">
	<link rel="stylesheet" href="../../_style/ssf.css">
	<script language="javascript" type="text/javascript" src="../../js/portal-all.js"></script>
	<script language="javascript" type="text/javascript">
	  var OF = null;
	  try {
		OF = self.opener.OF;
	  }
	  catch(e) {
		OF = null;
	  }
	  if(!OF) {
		OF = new ObjectFinder();
	  }
	</script>
	<script language="javascript" type="text/javascript" src="../../js/balloon/balloon.config.js"></script>
	<script language="javascript" type="text/javascript" src="../../js/balloon/balloon.js"></script>
	<script language="javascript" type="text/javascript">
		balloon = new Balloon;

		function trim(str) {
			str = str.replace(/^[ ]+(.*)$/, '$1'); // Trims leading spaces
			str = str.replace(/^(.*)[ ]+$/, '$1'); // Trims trailing spaces
			return str;
		}

		function validateForm(formName) {
			try {
				var els = Form.getElements(formName);
				for (i=0; i<els.length; i++) {
					if (els[i].name.indexOf('org:opencrx:kernel:activity1:Activity:name' ) >= 0) {
						if (trim(els[i].value).length > 0) {
							return true;
						} else {
							els[i].style.backgroundColor='#FFB69A';
							els[i].focus();
							return false;
						}
					}
				}
			} catch (e) {}
			return true;
		}

		function updateCss(theClass, element, value) {
			var cssRules;
			if (document.all) {
				cssRules = 'rules';
			}
			else if (document.getElementById) {
				cssRules = 'cssRules';
			}
			for (var S = 0; S < document.styleSheets.length; S++){
				for (var R = 0; R < document.styleSheets[S][cssRules].length; R++) {
					if (document.styleSheets[S][cssRules][R].selectorText == theClass) {
						document.styleSheets[S][cssRules][R].style[element] = value;
					}
				}
			}
		}
	</script>
  <link rel='shortcut icon' href='../../images/favicon.ico' />
	<style>
		html,body {
			margin:0;
			border:0;
			padding:1;
			background:white;
		}
		fieldset {
			background-color:#F2F2F2;;
			margin:10px 0px 10px 0px;
		}
		legend span {
			font-size:14px;
			vertical-align:baseline;
		}
		#wizMonth {
			text-align:center;
			white-space:nowrap;
		}
		<!-- Schedule -->
		.schedule {
			border-collapse:collapse;
		}
		#scheduleTable td {
			vertical-align:top;
		}
		#scheduleTable .schedule tr td {
			white-space:nowrap;
			vertical-align:middle;
			padding-right:2px;
		}
		#scheduleTable .schedule tr td img {
			margin-right:8px;
		}
		input.bookable {
			background-color:#C1C1FF;
		}
		input.bookable:hover {
			background-color:#80FF00;
		}
		input.booked {
			background-color:#80FF00;
		}
		input.booked:hover {
			background-color:#FF9900;
		}
		input.disabled {
			background-color:transparent;
		}
		input.disabled:hover {
			background-color:transparent;
		}
		.hidden {
			display:none;
		}
		.maxWidth {
			width:100%;
		}
		.slot {
			width:100%;
			padding:1px 0;
			border-style:solid;
			border-width:1px;
			border-color:#DDDDDD;
		}
		.trigger {
			vertical-align:middle;
			cursor:pointer;
			padding-right:5px;
		}
		td.ticks {
			vertical-align:middle;
			text-align:right;
			padding-right:20px;
		}
		td.ticks img {
		  vertical-align:top;
		}
		td.results {
			background-color:#F9FB95;
			border-top:1px solid grey;
			padding-top:5px;
			padding-bottom:5px;
		}
		#resultRowTop TD, #resultRowTop TH{
			border-bottom:1px solid #ccc;
			white-space:nowrap;
		}
	</style>
</head>
<body>
<div id="container">
<div id="wrap">
	<div id="header" style="height:90px;">
	 		<div id="logoTable">
			<table id="headerlayout">
			  <tr id="headRow">
				<td id="head" colspan="2">
				  <table id="info">
					<tr>
					  <td id="headerCellLeft"><img id="logoLeft" src="../../images/logoLeft.gif" alt="openCRX" title="" /></td>
					  <td id="headerCellSpacerLeft"></td>
					  <td id="headerCellMiddle">&nbsp;</td>
					  <td id="headerCellRight"><img id="logoRight" src="../../images/logoRight.gif" alt="" title="" /></td>
					</tr>
				  </table>
				</td>
			  </tr>
			</table>
		</div>
   	</div>
	<div id="content-wrap">
		<div id="content" style="padding:100px 0.5em 0px 0.5em;">
			<a name="<%= ANCHOR_TOP %>"></a>
			<div id="printButton" style="background-color:transparent;" onClick="javascript:yuiPrint();">&nbsp;</div>
			<h1><%= bundle.get("GeneralLabel") %></h1>
			<form id="<%= FORM_NAME %>" name="<%= FORM_NAME %>" method="post" accept-charset="UTF-8" action="<%= "../.." + request.getServletPath() %>">
				<input type="hidden" name="<%= Action.PARAMETER_REQUEST_ID %>" value="<%= requestId %>" />
				<input type="hidden" name="<%= Action.PARAMETER_OBJECTXRI %>" value="<%= objectXri %>" />
				<input type="hidden" id="Mode" name="Mode" value="<%= formValues.get("Mode") %>" />
				<input type="hidden" id="Command" readonly name="Command" value="" />
				<%= bundle.get("WizardDescription") %>
				<div>&nbsp;</div>
				<fieldset>
					<a name="<%= ANCHOR_CALENDAR %>"></a>
					<legend><span><%= bundle.get("EventLabel") %></span></legend>
<%
					if(!isEditMode) {
%>
						<%= bundle.get("EMailCreatorDescription") %>
						<table class="fieldGroup">
							<tr>
								<td class="<%= CssClass.fieldLabel %>">
									<span class="nw"><%= bundle.get("EMailCreatorLabel") %>:</span>
								</td>
								<td>
									<select id="emailCreator" name="emailCreator" class="valueL">
<%
										org.opencrx.kernel.activity1.jmi1.ActivityType activityTypeEMails = org.opencrx.kernel.backend.Activities.getInstance().findActivityType(
											org.opencrx.kernel.backend.Activities.ACTIVITY_TYPE_NAME_EMAILS,
											activitySegment,
											pm
										);
										org.opencrx.kernel.activity1.cci2.ActivityCreatorQuery emailCreatorQuery =
											(org.opencrx.kernel.activity1.cci2.ActivityCreatorQuery)pm.newQuery(org.opencrx.kernel.activity1.jmi1.ActivityCreator.class);
										emailCreatorQuery.thereExistsActivityType().equalTo(activityTypeEMails);
										emailCreatorQuery.orderByName().ascending();
										for(Iterator i = activitySegment.getActivityCreator(emailCreatorQuery).iterator(); i.hasNext(); ) {
											org.opencrx.kernel.activity1.jmi1.ActivityCreator emailCreator = (org.opencrx.kernel.activity1.jmi1.ActivityCreator)i.next();
%>
											<option <%= emailCreator.refGetPath().equals(formValues.get("emailCreator")) ? "selected" : "" %> value="<%= emailCreator.refMofId() %>"><%= new ObjectReference(emailCreator, app).getTitle() %></option>
<%
										}
%>
									</select>
								</td>
								<td class="gap"/>
							</tr>
						</table>
						<div>&nbsp;</div>
<%
					}
					form.paint(
						p,
						null, // frame
						true // forEditing
					);
					p.flush();

%>
					<div>&nbsp;</div>
				</fieldset>
<%
				// Dates and Times
				GregorianCalendar today = new GregorianCalendar(app.getCurrentLocale());
				today.setTimeZone(TimeZone.getTimeZone(app.getCurrentTimeZone()));
				Integer calendarYear = formValues.get("calendar.year") != null ?
					(Integer)formValues.get("calendar.year") :
					today.get(GregorianCalendar.YEAR);
				Integer calendarMonth = formValues.get("calendar.month") != null ?
					(Integer)formValues.get("calendar.month") :
					today.get(GregorianCalendar.MONTH);
				GregorianCalendar calendar = new GregorianCalendar(app.getCurrentLocale());
				calendar.setTimeZone(TimeZone.getTimeZone(app.getCurrentTimeZone()));
				calendar.setMinimalDaysInFirstWeek(4); // this conforms to DIN 1355/ISO 8601
				calendar.set(GregorianCalendar.YEAR, calendarYear);
				calendar.set(GregorianCalendar.MONTH, calendarMonth);
				calendar.set(GregorianCalendar.DAY_OF_MONTH, 1);
				SimpleDateFormat monthFormat = new java.text.SimpleDateFormat("MMMM", app.getCurrentLocale());
				SimpleDateFormat dayInWeekFormat = new java.text.SimpleDateFormat("E", app.getCurrentLocale());
				org.opencrx.kernel.activity1.jmi1.ActivityCreator tentativeCreator = null;
%>

				<!-- Event Schedule -->
				<fieldset>
					<legend><span><%= bundle.get("EventScheduleLabel") %></span></legend>
					<div>&nbsp;</div>
					<%= bundle.get("EventTrackerLabel") %>:
					<select id="eventTracker" name="eventTracker" onchange="javascript:$('RefreshCal.Button').click();">
						<option value=""><%= bundle.get("PleaseSelect") %></option>
<%
						org.opencrx.kernel.activity1.cci2.ActivityTrackerQuery eventTrackerQuery =
							(org.opencrx.kernel.activity1.cci2.ActivityTrackerQuery)pm.newQuery(org.opencrx.kernel.activity1.jmi1.ActivityTracker.class);
						eventTrackerQuery.orderByName().ascending();
						for(Iterator i = activitySegment.getActivityTracker(eventTrackerQuery).iterator(); i.hasNext(); ) {
							org.opencrx.kernel.activity1.jmi1.ActivityTracker eventTracker = (org.opencrx.kernel.activity1.jmi1.ActivityTracker)i.next();
							boolean isSelectedTracker = eventTracker.refGetPath().equals(formValues.get("eventTracker"));
							if(isSelectedTracker) {
								try {
									tentativeCreator = eventTracker.getDefaultCreator();
								} catch (Exception e) {}
							}
%>
							<option <%= isSelectedTracker ? "selected" : "none" %> value="<%= eventTracker.refGetPath().toXRI() %>"><%= new ObjectReference(eventTracker, app).getTitle() %></option>
<%
						}
%>
					</select>
<%
					if(tentativeCreator != null) {
%>
						(<%= bundle.get("TentativeEventCreatorLabel") %>: <%= tentativeCreator.getName() != null ? tentativeCreator.getName() : "?" %>)
<%
					}
%>
					<div>&nbsp;</div>
					<table id="scheduleTable">
						<tr>
							<!--  Calendar -->
							<td>
								<table>
									<tr><td>
										<div id="wizMonth" style="width:100%;">
											<table style="width:100%;">
												<tr>
													<td>
														<input id="Button.PrevYear" name="PrevYear" type="submit" tabindex="<%= tabIndex++ %>" class="<%= CssClass.btn.toString() %> <%= CssClass.btn_light.toString() %>" value="&lt;&lt;" onclick="<%= SUBMIT_HANDLER %>" />
														<input id="Button.PrevMonth" name="PrevMonth" type="submit" tabindex="<%= tabIndex++ %>" class="<%= CssClass.btn.toString() %> <%= CssClass.btn_light.toString() %>" value="&nbsp;&nbsp;&lt;&nbsp;" onclick="<%= SUBMIT_HANDLER %>"  />
													</td>
													<td style="width:100%;vertical-align:middle;">
														<span style="font-weight:bold;">&nbsp;<%= monthFormat.format(calendar.getTime()) + " " + calendarYear %>&nbsp;</span>
													</td>
													<td>
														<input id="Button.NextMonth" name="NextMonth" type="submit" tabindex="<%= tabIndex++ %>" class="<%= CssClass.btn.toString() %> <%= CssClass.btn_light.toString() %>" value="&nbsp;&gt;&nbsp;&nbsp;" onclick="<%= SUBMIT_HANDLER %>" />
														<input id="Button.NextYear" name="NextYear" type="submit" tabindex="<%= tabIndex++ %>" class="<%= CssClass.btn.toString() %> <%= CssClass.btn_light.toString() %>" value="&gt;&gt;" onclick="<%= SUBMIT_HANDLER %>"  />
														<input type="hidden" name="calendar.year" tabindex="<%= tabIndex++ %>" value="<%= calendarYear %>"/>
														<input type="hidden" name="calendar.month" tabindex="<%= tabIndex++ %>" value="<%= calendarMonth %>"/>
													</td>
												</tr>
											</table>
										</div>
									</td></tr>
									<tr><td>
										<table id="calWizard" cellspacing="1">
											<thead>
												<tr>
													<th style="text-align:center;padding:0px 10px;font-size:7pt;">#</th>
<%
													GregorianCalendar dayInWeekCalendar = (GregorianCalendar)calendar.clone();
													while(dayInWeekCalendar.get(GregorianCalendar.DAY_OF_WEEK) != dayInWeekCalendar.getFirstDayOfWeek()) {
														dayInWeekCalendar.add(GregorianCalendar.DAY_OF_MONTH, 1);
													}
													for(int i = 0; i < 7; i++) {
%>
														<th style="text-align:right;font-size:7pt;"><%= dayInWeekFormat.format(dayInWeekCalendar.getTime()) %>&nbsp;</th>
<%
														dayInWeekCalendar.add(GregorianCalendar.DAY_OF_MONTH, 1);
													}
%>
												</tr>
											</thead>
											<tbody>
<%
												Date yesterday = new Date(System.currentTimeMillis() - 86400000L);
												while(calendar.get(GregorianCalendar.MONTH) == calendarMonth) {
%>
													<tr>
														<td style="text-align:right;font-size:7pt;vertical-align:middle;padding:0px 10px;"><%= calendar.get(GregorianCalendar.WEEK_OF_YEAR) %></td>
<%
														for(int i = 0; i < 7; i++) {
															dayOfMonth = calendar.get(GregorianCalendar.DAY_OF_MONTH);
															if(((i + calendar.getFirstDayOfWeek() - 1) % 7 + 1) != calendar.get(GregorianCalendar.DAY_OF_WEEK)) {
%>
																<td>&nbsp;</td>
<%
															}
															else {
																if(calendar.get(GregorianCalendar.MONTH) != calendarMonth) {
%>
																	<td>&nbsp;</td>
<%
																}
																else if(calendar.getTime().compareTo(yesterday) < 0) {
%>
																	<td style="text-align:right;"><input type="button" value="<%= dayOfMonth < 10 ? "  " : "" %><%= dayOfMonth %>&nbsp;" class="<%= CssClass.btn.toString() + " " + CssClass.btn_light.toString() %> disabled" disabled /></td>
<%
																}
																else {
																	String dateAsString = getDateAsString(
																		calendarYear,
																		calendarMonth + 1,
																		dayOfMonth
																	);
																	if(selectedDates.contains(dateAsString)) {
																		dateIndex = selectedDates.indexOf(dateAsString);
%>
																		<td style="text-align:right;"><input id="DeleteDate.<%= dateIndex %>.Button" tabindex="<%= tabIndex++ %>" name="DeleteDate.<%= dateIndex %>" type="submit" class="<%= CssClass.btn.toString() + " " + CssClass.btn_light.toString() %> booked" value="<%= dayOfMonth < 10 ? "  " : "" %><%= dayOfMonth %>&nbsp;" onclick="<%= SUBMIT_HANDLER %>"/></td>
<%
																	}
																	else {
%>
																		<td style="text-align:right;"><input id="AddDate.<%= dayOfMonth %>.Button" tabindex="<%= tabIndex++ %>" name="AddDate.<%= dayOfMonth %>" type="submit" class="<%= CssClass.btn.toString() + " " + CssClass.btn_light.toString() %> bookable" value="<%= dayOfMonth < 10 ? "  " : "" %><%= dayOfMonth %>&nbsp;" onclick="<%= SUBMIT_HANDLER %>"/></td>
<%
																	}
																}
																calendar.add(GregorianCalendar.DAY_OF_MONTH, 1);
															}
														}
%>
													</tr>
<%
												}
%>
											</tbody>
										</table>
									</td></tr>
								</table>
							</td>
							<td><img src="../../images/spacer.gif" height="20" width="50"/></td>
							<!-- Selected dates -->
							<td class="maxWidth">
<%
								boolean makeHiddenVisible = false;
								if(!selectedDates.isEmpty()) {
									// Retrieve all events of eventTracker where scheduleStart/scheduledEnd
									// overlaps the period [first selected date, last selected date]
									org.w3c.format.DateTimeFormat dateFormat = org.w3c.format.DateTimeFormat.BASIC_UTC_FORMAT;
									Date start = new Date(getDateAsCalendar((String)selectedDates.get(0), app).getTime().getTime() - 86400000L);
									Date end = new Date(getDateAsCalendar((String)selectedDates.get(selectedDates.size() - 1), app).getTime().getTime() + 86400000L);
									org.opencrx.kernel.activity1.cci2.ActivityQuery eventQuery =
										(org.opencrx.kernel.activity1.cci2.ActivityQuery)pm.newQuery(org.opencrx.kernel.activity1.jmi1.Activity.class);
									eventQuery.thereExistsScheduledStart().lessThanOrEqualTo(end);
									eventQuery.thereExistsScheduledEnd().greaterThanOrEqualTo(start);
									eventQuery.forAllDisabled().isFalse();
									org.opencrx.kernel.activity1.jmi1.ActivityTracker eventTracker = formValues.get("eventTracker") != null ? 
										(org.opencrx.kernel.activity1.jmi1.ActivityTracker)pm.getObjectById(
											formValues.get("eventTracker")
										) : null;
									List events = eventTracker == null ?
										Collections.EMPTY_LIST :
										eventTracker.getFilteredActivity(eventQuery);
%>
									<div>
										<table class="schedule maxWidth">
											<thead>
												<tr>
													<th colspan="2"><%= timezone.getID() %></th>
<%
													for(int i = 0; i < NUM_SLOTS; i++) {
%>
														<th <%= i >= NUM_SLOTS_INITIALLY_VISIBLE ? "class='hidden' " : "" %>><%= bundle.get("Slot") + "&nbsp;" + (i+1) %></th>
														<th <%= i >= NUM_SLOTS_INITIALLY_VISIBLE ? "class='hidden' " : "" %> style="text-align:right;"><%= i+1 == NUM_SLOTS_INITIALLY_VISIBLE ? "<input id=\"Button.ExpandSlots\" name=\"ExpandSlots\" type=\"button\" tabindex=\"" + (tabIndex++) + "\" class=\"" + CssClass.btn.toString() + " " + CssClass.btn_light.toString() + "\" value=\"&gt;&gt;\" onclick=\"javascript:updateCss('.hidden', 'display', '');this.style.display='none';return false;\"  />" : "" %></th>
<%
													}
%>
												</tr>
											</thead>
											<tbody>
<%
												int ii = 0;
												for(Iterator i = selectedDates.iterator(); i.hasNext(); ii++) {
													String dateAsString = (String)i.next();
													GregorianCalendar date = getDateAsCalendar(dateAsString, app);
%>
													<tr>
														<td>
															<input id="DeleteDate.<%= ii %>.Button" name="DeleteDate.<%= ii %>" type="submit" tabindex="<%= tabIndex++ %>" class="<%= CssClass.btn.toString() %> <%= CssClass.btn_light.toString() %>" value="&ndash;" onclick="<%= SUBMIT_HANDLER %>" />
														</td>
														<td>&nbsp;&nbsp;<%= new SimpleDateFormat("EEE, MMMM d, yyyy", app.getCurrentLocale()).format(date.getTime()) %></td>
														<input type="hidden" name="calendar.selectedDate.<%= ii %>" value="<%= dateAsString %>"/>
<%
														for(int j = 0; j < NUM_SLOTS; j++) {
															String slotId = getSlotId(dateAsString, j);
															String event = (String)formValues.get(slotId);
%>
															<td <%= j >= NUM_SLOTS_INITIALLY_VISIBLE ? "class='hidden' " : "" %>>
																<input type="text" class="slot" name="<%= slotId %>" tabindex="<%= tabIndex++ %>" value="<%= formatEvent(event, timeFormat) %>" />
															</td>
															<td <%= j >= NUM_SLOTS_INITIALLY_VISIBLE ? "class='hidden' " : "" %>>
<%
																if(event != null && event.length() > 0) {
																	if (j >= NUM_SLOTS_INITIALLY_VISIBLE) {
																		makeHiddenVisible = true;
																	}
																	Date scheduledStart = dateFormat.parse(event.substring(0, 20));
																	Date scheduledEnd = event.length() < 41 ? dateFormat.parse(event.substring(0, 20)) : dateFormat.parse(event.substring(21, 41));
																	if(eventTracker != null) {
																		// Activity for event already created.
																		// If yes allow to remove it. If no allow to create a new activity.
																		org.opencrx.kernel.activity1.cci2.ActivityQuery activityExistsQuery =
																			(org.opencrx.kernel.activity1.cci2.ActivityQuery)pm.newQuery(org.opencrx.kernel.activity1.jmi1.Activity.class);
																			if(isEditMode) {
																				activityExistsQuery.thereExistsScheduledStart().greaterThanOrEqualTo(scheduledStart);
																				activityExistsQuery.thereExistsScheduledEnd().lessThanOrEqualTo(scheduledEnd);
																				activityExistsQuery.thereExistsExternalLink().equalTo(obj.refGetPath().getLastSegment().toString());
																				activityExistsQuery.forAllDisabled().isFalse();
																				if(!eventTracker.getFilteredActivity(activityExistsQuery).isEmpty()) {
%>
																					<input type="submit" id="DisableTentativeEvent.<%= event %>.Button" name="DisableTentativeEvent.<%= event %>" tabindex="<%= tabIndex++ %>" <%= eventTracker == null ? "disabled" : "" %> class="<%= CssClass.btn.toString() + " " + CssClass.btn_light.toString() %> booked"  title="<%= bundle.get("DisableTentativeEventTitle") %>" value="&ndash;" onclick="<%= SUBMIT_HANDLER %>"/>
<%
																				}
																				else {
%>
																					<input type="submit" id="CreateTentativeEvent.<%= event %>.Button" name="CreateTentativeEvent.<%= event %>" tabindex="<%= tabIndex++ %>" <%= eventTracker == null || tentativeCreator == null || isEventWithoutTime(event) ? "disabled" : "class=\"" + CssClass.btn.toString() + " " + CssClass.btn_light.toString() + " bookable\"" %> title="<%= bundle.get("AddTentativeEventTitle") %>" value="+" onclick="<%= SUBMIT_HANDLER %>"/>
<%
																				}
																			}
																		// Conflicts
																		org.opencrx.kernel.activity1.cci2.ActivityQuery eventHasConflictsQuery =
																			(org.opencrx.kernel.activity1.cci2.ActivityQuery)pm.newQuery(org.opencrx.kernel.activity1.jmi1.Activity.class);
																		eventHasConflictsQuery.thereExistsScheduledStart().lessThan(scheduledEnd);
																		eventHasConflictsQuery.thereExistsScheduledEnd().greaterThan(scheduledStart);
																		if(isEditMode) {
																			eventHasConflictsQuery.forAllExternalLink().notEqualTo(obj.refGetPath().getLastSegment().toString());
																		}
																		eventHasConflictsQuery.forAllDisabled().isFalse();
																		org.opencrx.kernel.activity1.jmi1.Activity conflictingActivity = null;
																		List conflictingActivities = eventTracker.getFilteredActivity(eventHasConflictsQuery);
																		if(!conflictingActivities.isEmpty()) {
																			conflictingActivity = (org.opencrx.kernel.activity1.jmi1.Activity)conflictingActivities.iterator().next();
																		}
																		if(conflictingActivity != null) {
																			ObjectReference activityRef = new ObjectReference(conflictingActivity, app);
																			String tooltip =
																				"<div style=\\'border-bottom:1px solid grey;padding-bottom:4px;margin-bottom:8px;\\'>" + bundle.get("ConflictLabel") + ":</div>" +
																				"<a href=\\'../../" + activityRef.getSelectObjectAction().getEncodedHRef(requestId) + "\\' target=\\'_blank\\'><div>" +
																				"<strong>" + (activityRef.getTitle()).replace(" ", "&nbsp;") + "</strong><br />" +
																				("Start: " + localizedDateTimeFormat.format(conflictingActivity.getScheduledStart())).replace(" ", "&nbsp;") + "<br />" +
																				("End: " + localizedDateTimeFormat.format(conflictingActivity.getScheduledEnd())).replace(" ", "&nbsp;") + "</div></a>";
%>
																			<img class="trigger" src="../../images/sAlert.gif" onclick="balloon.showTooltip(event,'<%= tooltip %>', 1)" />
<%
																		}
																	}
																}
%>
															</td>
<%
														}
%>
													</tr>
<%
												}
%>
											</tbody>
										</table>
									</div>
									<div>&nbsp;</div>
<%
								}
								if (makeHiddenVisible) {
%>
									<script language="javascript" type="text/javascript">
										updateCss('.hidden', 'display', '');
										$('Button.ExpandSlots').style.display='none';
									</script>
<%
								}
%>
								<input id="RefreshCal.Button" name="RefreshCal" type="submit" tabindex="<%= tabIndex++ %>" class="<%= CssClass.btn.toString() %> <%= CssClass.btn_light.toString() %>" value="<%= bundle.get("RefreshLabel") %>" onclick="<%= SUBMIT_HANDLER %>"/>
				 				<input id="ApplyCal.Button" name="ApplyCal" type="submit" tabindex="<%= tabIndex++ %>" class="<%= CssClass.btn.toString() %> <%= CssClass.btn_light.toString() %>" value="<%= bundle.get("ApplyLabel") %>" onclick="<%= SUBMIT_HANDLER_WITH_CHECK %>"/>
								<input id="CopyFirstRow.Button" <%= selectedDates.isEmpty() ? "style='display:none;" : "" %> name="CopyFirstRow" type="submit" tabindex="<%= tabIndex++ %>" class="<%= CssClass.btn.toString() %> <%= CssClass.btn_light.toString() %>" value="<%= bundle.get("CopyFirstRowLabel") %>" onclick="<%= SUBMIT_HANDLER %>"/>
							</td>
						</tr>
					</table>
					<%= bundle.get("EventScheduleDescription") %>
				</fieldset>
				<!-- Poll -->
				<fieldset>
					<legend><span><%= bundle.get("PollInformationLabel") %></span></legend>
					<div>&nbsp;</div>
					<table class="fieldGroup">
						<tr>
							<td class="<%= CssClass.fieldLabel %>">
								<span class="nw"><%= bundle.get("ClosedGroupPollLabel") %>:</span>
							</td>
							<td>
								<input type="checkbox" id="isClosedGroupPoll.Button" name="isClosedGroupPoll" <%= ((Boolean)formValues.get("isClosedGroupPoll")).booleanValue() ? "checked" : "" %>/>
							</td>
							<td class="gap"/>
						</tr>
						<tr>
							<td class="<%= CssClass.fieldLabel %>">
								<span class="nw"><%= bundle.get("HiddenPollLabel") %>:</span>
							</td>
							<td>
								<input type="checkbox" id="isHiddenPoll.Button" name="isHiddenPoll" <%= ((Boolean)formValues.get("isHiddenPoll")).booleanValue() ? "checked" : "" %>/>
							</td>
							<td class="gap"/>
						</tr>
						<tr>
							<td class="<%= CssClass.fieldLabel %>">
								<span class="nw"><%= bundle.get("YesNoPollLabel") %>:</span>
							</td>
							<td>
								<input type="checkbox" id="isYesNoPoll.Button" name="isYesNoPoll" <%= ((Boolean)formValues.get("isYesNoPoll")).booleanValue() ? "checked" : "" %> onClick="javascript:if($('isLimitTo1Poll.Button').checked){this.checked=true;return false;};" />
							</td>
							<td class="gap"/>
						</tr>
						<tr>
							<td class="<%= CssClass.fieldLabel %>">
								<span class="nw"><%= bundle.get("LimitOksPollLabel") %>:</span>
							</td>
							<td>
								<input type="checkbox" id="isLimitTo1Poll.Button" name="isLimitTo1Poll" <%= ((Boolean)formValues.get("isLimitTo1Poll")).booleanValue() ? "checked" : "" %> onClick="javascript:if(this.checked){$('isYesNoPoll.Button').checked=true;};" />
							</td>
							<td class="gap"/>
						</tr>
						<a name="<%= ANCHOR_PARTICIPANTS %>"></a>
						<tr>
							<td class="<%= CssClass.fieldLabel %>">
								<span class="nw"><%= bundle.get("MessageToVotersLabel") %>:</span>
							</td>
							<td>
<%
								String messageBody = (String)formValues.get("messageBody");
%>
								<textarea tabindex="<%= tabIndex++ %>" class="string" style="width: 100%;" rows="6" name="messageBody" id="messageBody"><%= messageBody == null ? "" : messageBody %></textarea>
							</td>
							<td class="gap"/>
						</tr>
					</table>
					<div>&nbsp;</div>
				</fieldset>
				<fieldset>
					<legend><span><%= bundle.get("VoterInformationLabel") %></span></legend>
					<div style="padding:5px;<%= actionNotifyVoter && actionNotifyVoterCompleted ? "" : "display:none;" %>"><%= bundle.get("AlertToVoterSuccess") %>: <b><%= notifyVoterUsername != null ? notifyVoterUsername : "" %></b></div>
<%
					if(voterCount > 0) {
						int lastVoterIndex = -1;
%>
						<!-- Participants -->
						<div>&nbsp;</div>
						<div class="fieldGroupName"><%= bundle.get("ClosedGroupVotersLabel") %></div>
						<table>
<%
							int i;
							for(i = 0; i < voterCount; i++) {
								if(formValues.get("voter." + i) != null) {
									lastVoterIndex = i;
									org.opencrx.kernel.account1.jmi1.EMailAddress emailAddress = formValues.get("voter." + i) != null ?
										(org.opencrx.kernel.account1.jmi1.EMailAddress)pm.getObjectById(
											formValues.get("voter." + i)
										) : null;
									String subject = formValues.get("org:opencrx:kernel:activity1:Activity:name") != null
									  ? java.net.URLEncoder.encode((String)formValues.get("org:opencrx:kernel:activity1:Activity:name"), "UTF-8").replace("+", "%20")
									  : "";
									String body = formValues.get("messageBody") != null
									  ? java.net.URLEncoder.encode((String)formValues.get("messageBody"), "UTF-8").replace("+", "%20")
									  : "";
									// Browser limit
									if(messageBody != null && messageBody.length() > 1500) {
										messageBody = messageBody.substring(0, 1500);
									}
									if(i % 4 == 0) {
%>
										<tr>
<%
									}
%>
									<td><input class="<%= CssClass.btn.toString() %> <%= CssClass.btn_light.toString() %>" type="submit" id="DeleteVoter.<%= i %>.Button" name="DeleteVoter.<%= i %>" tabindex="<%= tabIndex++ %>" title="<%= bundle.get("RemoveVoterTitle") %>" value="&ndash;" onclick="<%= SUBMIT_HANDLER %>"/></td>
									<td style="vertical-align:middle">
										<a href="mailto:<%= emailAddress.getEmailAddress() + "?subject=" + subject + "&body=" + body %>" title="<%= bundle.get("EmailToVoterTitle") %>"><%= emailAddress.getEmailAddress() %></a><input type="hidden" name="voter.<%= i %>" value="<%= emailAddress.refMofId() %>"/>
<%
										String userName = getUsername(pm, homeSegment, emailAddress.getEmailAddress());
										if (userName != null) {
%>
											<button type="submit" id="NotifyVoter.<%= i %>.<%= userName %>.Button" name="NotifyVoter.<%= i %>.<%= userName %>" title="<%= bundle.get("AlertToVoterTitle") %>: <%= userName %>" class="trigger" tabindex="<%= tabIndex++ %>" value="&mdash;" onclick="<%= SUBMIT_HANDLER %>" style="border:0; background:transparent;" ><img src="../../images/sAlert.gif" /></button>
<%
										}
%>
									</td>
									<td class="addon"/>
<%
									if(i % 4 == 3) {
%>
										</tr>
<%
									}
								}
							}
							while(i % 4 != 3) {
%>
								<td />
								<td />
								<td />
<%
								i++;
							}
%>
						</table>
<%
						voterCount = lastVoterIndex + 1;
					}
%>
					<!-- Add Voter -->
					<div>&nbsp;</div>
<%
					addVoterForm.paint(
						p,
						null, // frame
						true // forEditing
					);
					p.flush();
%>
					<input type="hidden" name="voter.count" id="voter.count" value="<%= voterCount %>" />
					<input type="submit" class="<%= CssClass.btn.toString() %> <%= CssClass.btn_light.toString() %>" name="AddVoter" id="AddVoter.Button" tabindex="<%= tabIndex++ %>" title="<%= bundle.get("AddSelectedVoterTitle") %>" value="+" onclick="<%= SUBMIT_HANDLER %>" />
				</fieldset>
				<a name="<%= ANCHOR_VOTES %>"></a>
<%
				if(obj instanceof org.opencrx.kernel.activity1.jmi1.EMail) {
					org.opencrx.kernel.activity1.jmi1.EMail email = (org.opencrx.kernel.activity1.jmi1.EMail)obj;
%>
					<fieldset>
						<legend><span><%= bundle.get("VotesLabel") %></span></legend>
						<div>&nbsp;</div>
							<table cellspacing="1">
								<tr>
									<th />
									<th />
<%
									// Table head for Dates
									for(int i = 0; i < selectedDates.size(); i++) {
										String dateAsString = (String)selectedDates.get(i);
										int colspan = 0;
										for(int j = 0; j < NUM_SLOTS; j++) {
											String slotId = getSlotId(dateAsString, j);
											if((formValues.get(slotId)) != null && (((String)formValues.get(slotId)).length() > 0)) {
												colspan++;
											}
										}
										GregorianCalendar date = getDateAsCalendar(dateAsString, app);
%>
										<th style="background-color: lightblue" colspan="<%= colspan %>">&nbsp;<%= new SimpleDateFormat("EEE, MMMM d, yyyy", app.getCurrentLocale()).format(date.getTime()) %>&nbsp;</th>
<%
									}
%>
								</tr>
								<tr>
									<th />
									<th />
<%
									// Table head for Slots
									int nSlots = 0;
									for(int i = 0; i < selectedDates.size(); i++) {
										String dateAsString = (String)selectedDates.get(i);
										for(int j = 0; j < NUM_SLOTS; j++) {
											String slotId = getSlotId(dateAsString, j);
											String event = (String)formValues.get(slotId);
											if(event != null && event.length() > 0) {
%>
												<th><input type="submit" id="CreateConfirmedEvent.<%= event %>.Button" name="CreateConfirmedEvent.<%= event %>" <%= isEditMode ? "" : "disabled" %> class="<%= CssClass.btn.toString() %> <%= CssClass.btn_light.toString() %>" value="<%= formatEvent(event, timeFormat) %>" title="<%= bundle.get("CreateConfirmedEventTitle") %>" onclick="<%= SUBMIT_HANDLER %>"/></th>
<%
												nSlots++;
											}
										}
									}
%>
								</tr>
								<tr id="resultRowTop">
									<th />
									<th align="right">
										<%= bundle.get("yesLabel") %>&nbsp;<br>
										<%= ((Boolean)formValues.get("isYesNoPoll")).booleanValue() ? "" : bundle.get("ifneedbeLabel") + "&nbsp;<br>" %>
										<%= bundle.get("noLabel") %>&nbsp;
									</th>
<%
									List yesVotes = new ArrayList();
									List noVotes = new ArrayList();
									List ifneedbeVotes = new ArrayList();
									// Place holders for total votes
									for(int i = 0; i < nSlots; i++) {
										yesVotes.add(new Integer(0));
										noVotes.add(new Integer(0));
										ifneedbeVotes.add(new Integer(0));
%>
										<td class="results ticks"><span id="slot<%= i %>Votes"></span></td>
<%
									}
%>
								</tr>
<%
								int ii = 0;
								org.opencrx.kernel.activity1.cci2.ActivityVoteQuery voteQuery = (org.opencrx.kernel.activity1.cci2.ActivityVoteQuery)pm.newQuery(org.opencrx.kernel.activity1.jmi1.ActivityVote.class);
								voteQuery.orderByName().ascending();
								for(Iterator i = email.getVote(voteQuery).iterator(); i.hasNext(); ii++) {
									org.opencrx.kernel.activity1.jmi1.ActivityVote vote = (org.opencrx.kernel.activity1.jmi1.ActivityVote)i.next();
									ByteArrayInputStream in = new ByteArrayInputStream(vote.getDescription().getBytes("UTF-8"));
									Properties votes = new Properties();
									votes.loadFromXML(in);
									in.close();
%>
									<tr <%= ii % 2 == 0 ? "style='background-color:#F4F4F4;'" : "" %>>
										<td><img src="../../images/spacer.gif" height="20" width="0"/></td>
										<td style="vertical-align:middle"><%= new ObjectReference(vote, app).getTitle() %></td>
<%
										int nSlot = 0;
										for(int j = 0; j < selectedDates.size(); j++) {
											String dateAsString = (String)selectedDates.get(j);
											for(int k = 0; k < NUM_SLOTS; k++) {
												String slotId = getSlotId(dateAsString, k);
												String event = (String)formValues.get(slotId);
												if((event != null && (event.length() > 0))) {
%>
													<td class="ticks">
<%
														if(votes.get(event) != null) {
															if ("2".equals(votes.get(event)) || "true".equals(votes.get(event)) || "on".equals(votes.get(event))) {
																yesVotes.set(nSlot, ((Integer)yesVotes.get(nSlot))+1);
%>
																<%= "<img src=\"../../images/checked.gif\"/>" %>
<%
															}
															else if ("1".equals(votes.get(event))) {
																ifneedbeVotes.set(nSlot, ((Integer)ifneedbeVotes.get(nSlot))+1);
%>
																<%= "<img src=\"../../images/ifneedbe.gif\"/>" %>
<%
															}
															else {
																noVotes.set(nSlot, ((Integer)noVotes.get(nSlot))+1);
%>
																<%= "<img src=\"../../images/notchecked.gif\"/>" %>
<%
															}
														}
%>
												  	</td>
<%
													nSlot++;
												}
											}
										}
%>
									</tr>
<%
								}
%>
								<tr id="resultRowBottom" style="display:none;">
									<th class="results" />
									<th align="right">
										<%= bundle.get("yesLabel") %>&nbsp;<br>
										<%= ((Boolean)formValues.get("isYesNoPoll")).booleanValue() ? "" : bundle.get("ifneedbeLabel") + "&nbsp;<br>" %>
										<%= bundle.get("noLabel") %>&nbsp;
									</th>
<%
									// Place holders for total votes
									for(int i = 0; i < nSlots; i++) {
										String results =
											"<div><b>" + ((Integer)yesVotes.get(i) == 0 ? "&nbsp;" : (Integer)yesVotes.get(i)) + " </b><img src=\"../../images/checked.gif\"/></div>" +
											(((Boolean)formValues.get("isYesNoPoll")).booleanValue()
												? ""
												: "<div><b>" + ((Integer)ifneedbeVotes.get(i) == 0 ? "&nbsp;" : (Integer)ifneedbeVotes.get(i)) + " </b><img src=\"../../images/ifneedbe.gif\"/></div>"
											) +
											"<div><b>" + ((Integer)noVotes.get(i) == 0 ? "&nbsp;" : (Integer)noVotes.get(i)) + " </b><img src=\"../../images/notchecked.gif\"/></div>";
%>
										<td class="results ticks">
											<%= results %>
											<script language="javascript" type="text/javascript">
												<%= ii == 0 ? "$('resultRowTop').style.display='none';" : "$('slot" + i + "Votes').replace('" + results + "');" %>
											</script>
										</td>
<%
									}
%>
								</tr>
							</table>
						<div>&nbsp;</div>
					</fieldset>
<%
				}
%>
 				<br />
				<a name="<%= ANCHOR_BOTTOM %>"></a>
 				<input id="Refresh.Button" name="Refresh" type="submit" tabindex="<%= tabIndex++ %>" class="<%= CssClass.btn.toString() %> <%= CssClass.btn_light.toString() %>" value="<%= bundle.get("RefreshLabel") %>" onclick="<%= SUBMIT_HANDLER %>"/>
 				<input id="Apply.Button" name="Apply" type="submit" tabindex="<%= tabIndex++ %>" class="<%= CssClass.btn.toString() %> <%= CssClass.btn_light.toString() %>" value="<%= bundle.get("ApplyLabel") %>" onclick="<%= SUBMIT_HANDLER_WITH_CHECK %>"/>
 				<input id="Save.Button" name="Save" type="submit" tabindex="<%= tabIndex++ %>" class="<%= CssClass.btn.toString() %> <%= CssClass.btn_light.toString() %>" value="<%= texts.getSaveTitle() %>" onclick="<%= SUBMIT_HANDLER_WITH_CHECK %>"/>
 				<input id="Cancel.Button" name="Cancel" type="submit" tabindex="<%= tabIndex++ %>" class="<%= CssClass.btn.toString() %> <%= CssClass.btn_light.toString() %>" value="<%= texts.getCloseText() %>" onclick="<%= SUBMIT_HANDLER %>"/>
 				<br />
 				<br />
			</form>
		</div> <!-- content -->
	</div> <!-- content-wrap -->
  </div> <!-- wrap -->
</div> <!-- container -->
<script language="javascript" type="text/javascript">
<%
	if (anchor != null) {
%>
		window.location.hash="<%= anchor %>";
<%
	}
%>
</script>
</body>
<%
p.close(false);
if(pm != null) {
	pm.close();
}
%>
</html>
