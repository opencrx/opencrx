<%@	page contentType="text/html;charset=UTF-8" language="java" pageEncoding="UTF-8" %><%
/**
 * ====================================================================
 * Project:     openCRX/Core, http://www.opencrx.org/
 * Description: Create Work And Expense Report
 * Owner:       the original authors.
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
java.math.*,
java.net.*,
java.text.*,
javax.xml.datatype.*,
org.openmdx.base.accessor.jmi.cci.*,
org.openmdx.base.exception.*,
org.openmdx.portal.servlet.*,
org.openmdx.portal.servlet.action.*,
org.openmdx.portal.servlet.attribute.*,
org.openmdx.portal.servlet.component.*,
org.openmdx.portal.servlet.control.*,
org.openmdx.portal.servlet.wizards.*,
org.openmdx.base.naming.*,
org.openmdx.base.query.*,
org.openmdx.kernel.id.cci.*,
org.openmdx.kernel.id.*,
org.openmdx.base.exception.*,
org.openmdx.base.text.conversion.*,
org.apache.poi.hssf.usermodel.*,
org.apache.poi.ss.usermodel.*,
org.apache.poi.hssf.util.*
" %>
<%!

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
		return // YYYYMMDD
			Integer.toString(year) +
			((month < 10 ? "0" : "") + Integer.toString(month)) +
			((dayOfMonth < 10 ? "0" : "") + Integer.toString(dayOfMonth));
	}

	public static GregorianCalendar getDateAsCalendar(
		String dateAsString, // YYYYMMDD
												 // 01234567
		ApplicationContext app
	) {
		GregorianCalendar date = new GregorianCalendar(app.getCurrentLocale());
		date.setTimeZone(TimeZone.getTimeZone(app.getCurrentTimeZone()));
		date.setMinimalDaysInFirstWeek(4); // this conforms to DIN 1355/ISO 8601
		date.set(GregorianCalendar.YEAR, Integer.valueOf(dateAsString.substring(0, 4)));
		date.set(GregorianCalendar.MONTH, Integer.valueOf(dateAsString.substring(4, 6)) - 1);
		date.set(GregorianCalendar.DAY_OF_MONTH, Integer.valueOf(dateAsString.substring(6)));
		date.set(GregorianCalendar.HOUR_OF_DAY, 0);
		date.set(GregorianCalendar.MINUTE, 0);
		date.set(GregorianCalendar.SECOND, 0);
		date.set(GregorianCalendar.MILLISECOND, 0);
		return date;
	}

	public static GregorianCalendar getDateTimeAsCalendar(
		String dateAsString, // dd-MM-YYYY HH:mm
												 // 0123456789012345
		ApplicationContext app
	) {
		GregorianCalendar date = new GregorianCalendar(app.getCurrentLocale());
		date.setTimeZone(TimeZone.getTimeZone(app.getCurrentTimeZone()));
		date.setMinimalDaysInFirstWeek(4); // this conforms to DIN 1355/ISO 8601
		try {
			date.set(GregorianCalendar.YEAR, Integer.valueOf(dateAsString.substring(6, 10)));
			date.set(GregorianCalendar.MONTH, Integer.valueOf(dateAsString.substring(3, 5)) - 1);
			date.set(GregorianCalendar.DAY_OF_MONTH, Integer.valueOf(dateAsString.substring(0, 2)));
			date.set(GregorianCalendar.HOUR_OF_DAY, Integer.valueOf(dateAsString.substring(11, 13)));
			date.set(GregorianCalendar.MINUTE, Integer.valueOf(dateAsString.substring(14)));
			date.set(GregorianCalendar.SECOND, 0);
			date.set(GregorianCalendar.MILLISECOND, 0);
		} catch (Exception e) {
				date = null;
		}
		return date;
	}

	public static int getDayOfWeek(
			String dateAsString,
			ApplicationContext app
		) {
			GregorianCalendar date = getDateAsCalendar(dateAsString, app);
			return date.get(date.DAY_OF_WEEK);
	}

	private static String decimalMinutesToHhMm(
		double decimalMinutes
	) {
		NumberFormat hhFormatter = new DecimalFormat("#,##0");
		NumberFormat mmFormatter = new DecimalFormat("#,#00");
		int hours = (int)(decimalMinutes / 60.0);
		int minutes = (int)java.lang.Math.rint(decimalMinutes % 60.0);
		if (minutes == 60) {
				hours += 1;
				minutes = 0;
		}
		return hhFormatter.format(hours) + ":" + mmFormatter.format(minutes);
	}
	
	public static String resourceErrorMsgUpdate(
		String resourceErrorMsg,
		String calDayName,
		Set resourcesToday,
		Map dayPercentageTotals,
		Map dayLoads,
		javax.jdo.PersistenceManager pm
	) {
		NumberFormat formatter0 = new DecimalFormat("0");
		for (Iterator r = resourcesToday.iterator(); r.hasNext(); ) {
			String resXri = (String)r.next();
			String dayKey = calDayName + resXri;
			org.opencrx.kernel.activity1.jmi1.Resource res = null;
			try {
					res = (org.opencrx.kernel.activity1.jmi1.Resource)pm.getObjectById(new Path(resXri));
			} catch (Exception e) {}
			if (res != null && (dayPercentageTotals.get(dayKey) != null) && ((Double)dayPercentageTotals.get(dayKey)).doubleValue() != 100.0) {
					if (resourceErrorMsg == null) {
							resourceErrorMsg = "total allocation not equal to 100%";
					}
					resourceErrorMsg += " | " + res.getName() + " [" + formatter0.format(((Double)dayPercentageTotals.get(dayKey)).doubleValue()) + "%]";
			}
		}
		return resourceErrorMsg;
	}

	private static HSSFSheet addSheet(
		HSSFWorkbook wb,
		String sheetName,
		boolean isLandscape,
		String[] labels,
		String[] values,
		int colsBetweenLabelsAndValues
	) {
			HSSFSheet sheet = wb.createSheet(sheetName);
			sheet.setMargin(HSSFSheet.TopMargin,		0.5);
			sheet.setMargin(HSSFSheet.RightMargin,	0.3);
			sheet.setMargin(HSSFSheet.BottomMargin, 0.6);
			sheet.setMargin(HSSFSheet.LeftMargin,	 0.5);
			sheet.setAutobreaks(true);


			HSSFPrintSetup ps = sheet.getPrintSetup();
			/*
			ps.setFitHeight((short)100);
			ps.setFitWidth((short)1);
			*/
			ps.setPaperSize(HSSFPrintSetup.A4_PAPERSIZE);
			ps.setLandscape(isLandscape);
			ps.setFooterMargin(0.3);

			HSSFFooter footer = sheet.getFooter();
			footer.setRight(HSSFFooter.page() + " / " + HSSFFooter.numPages());
			if (values.length > 3) {
					footer.setLeft(values[3]);
			}


			HSSFRow row = null;
			HSSFCell cell = null;
			short nRow = 0;
			short nCell = 0;
			nRow = 0;
			for (int i=0; i<labels.length; i++) {
					row = sheet.createRow(nRow++);
					nCell = 0;
					cell = row.createCell(nCell++);
					cell.setCellValue(labels[i]);
					cell = row.createCell(nCell++);
					for (int c = 0; c < colsBetweenLabelsAndValues; c++) {
							cell = row.createCell(nCell++);
					}
					cell.setCellValue(values[i]);
			}

			return sheet;
	}

%>
<%
	final int REPORT_STARTING_ROW = 6;
	final int MAX_ACTIVITY_SHOWN_INITIALLY = 50;
	final int MAX_ACTIVITY_SHOWN = 500;
	final int MAX_ACTIVITY_SORT_ORDER = 4;
	final String FORM_NAME = "WorkAndExpenseReport";
	final String WIZARD_NAME = FORM_NAME + ".jsp";
	final String SUBMIT_HANDLER = "javascript:$('command').value=this.name;";
	final String CAUTION = "<img border='0' alt='' height='16px' src='../../images/caution.gif' />";
	final String SPREADSHEET = "<img border='0' alt=''	height='32px' src='../../images/spreadsheet.png' />";
	final String GAP_BEFORE_XRI = "		 ";

	final String ACTIVITY_FILTER_SEGMENT = "Segment";
	final String ACTIVITY_FILTER_ANYGROUP = "AnyGroup";
	final String ACTIVITY_FILTER_FILTERGLOBAL = "ActivityFilterGlobal";
	final String ACTIVITY_FILTER_TRACKER = "Tracker";
	final String ACTIVITY_FILTER_PROJECT = "Project";
	final String ACTIVITY_FILTER_CATEGORY = "Category";
	final String ACTIVITY_FILTER_MILESTONE = "Milestone";

	final String ACTIVITY_CLASS = "org:opencrx:kernel:activity1:Activity";
	final String ACTIVITYFILTER_CLASS = "org:opencrx:kernel:activity1:ActivityFilterGlobal";
	final String ACTIVITYSEGMENT_CLASS = "org:opencrx:kernel:activity1:Segment";
	final String ACTIVITYGROUPASSIGNMENT_CLASS = "org:opencrx:kernel:activity1:ActivityGroupAssignment";
	final String ACTIVITYFILTERGLOBAL_CLASS = "org:opencrx:kernel:activity1:ActivityFilterGlobal";
	final String ACTIVITYTRACKER_CLASS = "org:opencrx:kernel:activity1:ActivityTracker";
	final String ACTIVITYCATEGORY_CLASS = "org:opencrx:kernel:activity1:ActivityCategory";
	final String ACTIVITYMILESTONE_CLASS = "org:opencrx:kernel:activity1:ActivityMilestone";
	final String DISABLED_FILTER_PROPERTY_CLASS = "org:opencrx:kernel:activity1:DisabledFilterProperty";
	final String RESOURCE_CLASS = "org:opencrx:kernel:activity1:Resource";
	final String ACCOUNT_CLASS = "org:opencrx:kernel:account1:Account";
	final String CONTACT_CLASS = "org:opencrx:kernel:account1:Contact";
	final String GROUP_CLASS = "org:opencrx:kernel:account1:Group";
	final String WORKANDEXPENSERECORD_CLASS = "org:opencrx:kernel:activity1:WorkAndExpenseRecord";
	final String CALENDAR_CLASS = "org:opencrx:kernel:activity1:Calendar";
	final int RECORDTYPE_WORK_MAX = 99; // <=99 --> WorkRecord, >= 100 --> ExpenseRecord
	final String[] UOM_NAMES = {
			"s", "min", "hour", "day",
			"m", "km", "mile", "feet", "inch",
			"kg",
			"Piece(s)", "Unit(s)"
	};

	final String FEATURE_RECORD_TYPE = "workAndExpenseType";
	final String FEATURE_PRIORITY = "priority";
	final String FEATURE_BILLING_CURRENCY = "currency";

	final String FEATURE_CONTACT_TARGET_FINDER = "org:opencrx:kernel:activity1:Resource:contact";

	final String DEFAULT_CURRENCY = "N/A";

	final String ERROR_STYLE = "style='background-color:#FFF0CC;'";
	final String ERROR_STYLEInline = "background-color:#FFF0CC;";

	// Init
	request.setCharacterEncoding("UTF-8");
	ApplicationContext app = (ApplicationContext)session.getValue(WebKeys.APPLICATION_KEY);
	ViewsCache viewsCache = (ViewsCache)session.getValue(WebKeys.VIEW_CACHE_KEY_SHOW);
	String requestId =	request.getParameter(Action.PARAMETER_REQUEST_ID);
	String objectXri = request.getParameter(Action.PARAMETER_OBJECTXRI);
	javax.jdo.PersistenceManager pm = app.getNewPmData();
	String requestIdParam = Action.PARAMETER_REQUEST_ID + "=" + requestId;
	String xriParam = Action.PARAMETER_OBJECTXRI + "=" + objectXri;
	if(objectXri == null || app == null || viewsCache.getView(requestId) == null) {
		session.setAttribute(WIZARD_NAME, null);
		response.sendRedirect(
			request.getContextPath() + "/" + WebKeys.SERVLET_NAME
		);
		return;
	}
	Texts_1_0 texts = app.getTexts();
	org.openmdx.portal.servlet.Codes codes = app.getCodes();

	RefObject_1_0 obj = (RefObject_1_0)pm.getObjectById(new Path(objectXri));
	String providerName = obj.refGetPath().get(2);
	String segmentName = obj.refGetPath().get(4);

	String errorMsg = "";

	// Format DateTimes
	TimeZone timezone = TimeZone.getTimeZone(app.getCurrentTimeZone());
	SimpleDateFormat dtf = new SimpleDateFormat("EEEE", app.getCurrentLocale());										dtf.setTimeZone(timezone);
	SimpleDateFormat monthFormat = new java.text.SimpleDateFormat("MMMM", app.getCurrentLocale());	monthFormat.setTimeZone(timezone);
	SimpleDateFormat dayInWeekFormat = new java.text.SimpleDateFormat("E", app.getCurrentLocale()); dayInWeekFormat.setTimeZone(timezone);
	SimpleDateFormat weekdayf = new SimpleDateFormat("EE", app.getCurrentLocale());									weekdayf.setTimeZone(timezone);
	SimpleDateFormat yyyyf = new SimpleDateFormat("yyyy", app.getCurrentLocale());									yyyyf.setTimeZone(timezone);
	SimpleDateFormat dateonlyf = new SimpleDateFormat("dd-MMM-yyyy", app.getCurrentLocale());				dateonlyf.setTimeZone(timezone);
	SimpleDateFormat datetimef = new SimpleDateFormat("dd-MMM-yyyy HH:mm", app.getCurrentLocale());	datetimef.setTimeZone(timezone);
	SimpleDateFormat calendardayf = new SimpleDateFormat("yyyyMMdd", app.getCurrentLocale());				calendardayf.setTimeZone(timezone);
	SimpleDateFormat jsCalenderf = new SimpleDateFormat("dd-MM-yyyy HH:mm", app.getCurrentLocale());jsCalenderf.setTimeZone(timezone);
	SimpleDateFormat datef = new SimpleDateFormat("EE d-MMMM-yyyy", app.getCurrentLocale());				datef.setTimeZone(timezone);
	SimpleDateFormat dtsortf = new SimpleDateFormat("yyyyMMddHHmmss", app.getCurrentLocale());			dtsortf.setTimeZone(timezone);
	SimpleDateFormat selectorf = new java.text.SimpleDateFormat("MM/yyyy", app.getCurrentLocale());	selectorf.setTimeZone(timezone);
	NumberFormat formatter2 = new DecimalFormat("00");
	NumberFormat formatter3 = new DecimalFormat("000");
	NumberFormat formatter = new DecimalFormat("00000");
	NumberFormat quantityf = new DecimalFormat("0.000");
	NumberFormat ratesepf = new DecimalFormat("#,##0.00");
	NumberFormat ratesehpf = new DecimalFormat("#,##0.00000");
	NumberFormat formatter0 = new DecimalFormat("0");
	DecimalFormat decimalFormat = (DecimalFormat)DecimalFormat.getInstance(app.getCurrentLocale());

	org.opencrx.kernel.activity1.jmi1.Segment activitySegment = (org.opencrx.kernel.activity1.jmi1.Segment)pm.getObjectById(
			new Path("xri:@openmdx:org.opencrx.kernel.activity1/provider/" + providerName + "/segment/" + segmentName)
		);

	org.opencrx.kernel.uom1.jmi1.Uom uomPercent = null;
	try {
		uomPercent = (org.opencrx.kernel.uom1.jmi1.Uom)pm.getObjectById(
				new Path("xri://@openmdx*org.opencrx.kernel.uom1/provider/" + providerName + "/segment/Root/uom/Percent")
		);
	} catch (Exception e) {}

	// Dates and Times
	Map formValues = new HashMap();

	UserDefinedView userView = new UserDefinedView(
		pm.getObjectById(new Path(objectXri)),
		app,
		viewsCache.getView(requestId)
	);
	int tabIndex = 1;

	boolean isFirstCall = request.getParameter("isFirstCall") == null; // used to properly initialize various options

	String command = request.getParameter("command");
	//System.out.println("command=" + command);
	boolean actionNextMonth = "NextMonth".equals(command);
	boolean actionPrevMonth = "PrevMonth".equals(command);
	boolean actionNextYear = "NextYear".equals(command);
	boolean actionPrevYear = "PrevYear".equals(command);
	boolean actionSelectDate = command != null && command.startsWith("SelectDate.");
	boolean actionSelectDateP = command != null && command.startsWith("SelectDateP.");
	boolean actionSelectDateN = command != null && command.startsWith("SelectDateN.");
	boolean actionCancel = command != null && command.startsWith("cancel.");
	boolean actionEvictAndReload = command != null && command.startsWith("EVICT_RELOAD");
	boolean actionShowReport = command != null && command.startsWith("ShowReport.");

	if (actionEvictAndReload) {
			app.resetPmData();
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

	boolean isWorkRecord = ((request.getParameter("isExpenseRecord") == null) || (request.getParameter("isExpenseRecord").length() == 0));
	boolean hasProjects = ((request.getParameter("hasProjects") != null) && (request.getParameter("hasProjects").length() > 0));

	if (request.getParameter("previousSheet") != null) {
			// delete previous temp file if it exists
			try {
					File previousFile = new File(
						app.getTempFileName(request.getParameter("previousSheet"), "")
					);
					if (previousFile.exists()) {
							previousFile.delete();
					}
			} catch (Exception e){
					new ServiceException(e).log();
			}
	}
	String sheetName = (isWorkRecord ? "Work_" : "Expense_") + "Report";
	final String location = UUIDConversion.toUID(UUIDs.newUUID());
	File f = new File(
		app.getTempFileName(location, "")
	);
	FileOutputStream os = new FileOutputStream(f);

	HSSFWorkbook wb = new HSSFWorkbook();

	HSSFCellStyle dateTimeStyle = wb.createCellStyle();
	HSSFDataFormat dataFormatDateTime = wb.createDataFormat();
	dateTimeStyle.setDataFormat(dataFormatDateTime.getFormat("dd-mmm-yyyy hh:mm"));

	HSSFCellStyle dateStyle = wb.createCellStyle();
	HSSFDataFormat dataFormatDate = wb.createDataFormat();
	dateStyle.setDataFormat(dataFormatDate.getFormat("dd-mmm-yyyy"));

	HSSFCellStyle timeStyle = wb.createCellStyle();
	HSSFDataFormat dataFormatTime = wb.createDataFormat();
	timeStyle.setDataFormat(dataFormatTime.getFormat("[h]:mm"));
	timeStyle.setAlignment(HorizontalAlignment.RIGHT);

	HSSFCellStyle rightAlignStyle = wb.createCellStyle();
	rightAlignStyle.setAlignment(HorizontalAlignment.RIGHT);

	HSSFCellStyle quantityStyle = wb.createCellStyle();
	quantityStyle.setAlignment(HorizontalAlignment.RIGHT);
	HSSFDataFormat format = wb.createDataFormat();
	quantityStyle.setDataFormat(format.getFormat("#,##0"));

	HSSFCellStyle amountStyle = wb.createCellStyle();
	amountStyle.setAlignment(HorizontalAlignment.RIGHT);
	HSSFDataFormat amountFormat = wb.createDataFormat();
	amountStyle.setDataFormat(amountFormat.getFormat("#,##0.00"));

	HSSFCellStyle weightStyle = wb.createCellStyle();
	weightStyle.setAlignment(HorizontalAlignment.RIGHT);
	HSSFDataFormat wformat = wb.createDataFormat();
	weightStyle.setDataFormat(wformat.getFormat("#,##0.000"));

	HSSFCellStyle percentStyle = wb.createCellStyle();
	percentStyle.setAlignment(HorizontalAlignment.RIGHT);
	HSSFDataFormat pformat = wb.createDataFormat();
	percentStyle.setDataFormat(wformat.getFormat("0.00%"));

	String contactXri = null;
	String resourceXri = null;
	String activityFilter = null;
	String activityFilterXri = null;
	String activityXri = null;
	String selector	= request.getParameter("selector") == null ? "" : request.getParameter("selector");
	boolean isSelectorChange = ((request.getParameter("isSelectorChange") != null) && (request.getParameter("isSelectorChange").length() > 0));

	if (isFirstCall) {
		try {
			// try to derive initial settings from calling object
			if (obj instanceof org.opencrx.kernel.account1.jmi1.Contact) {
					// called from Contact
					contactXri = ((org.opencrx.kernel.account1.jmi1.Contact)obj).refMofId();
			} else if (obj instanceof org.opencrx.kernel.activity1.jmi1.ActivityFilterGlobal) {
					// called from ActivityFilterGlobal
					activityFilterXri = ((org.opencrx.kernel.activity1.jmi1.ActivityFilterGlobal)obj).refMofId();
					activityFilter = ACTIVITY_FILTER_FILTERGLOBAL;
			} else if (obj instanceof org.opencrx.kernel.activity1.jmi1.ActivityTracker) {
					// called from ActivityTracker
					activityFilterXri = ((org.opencrx.kernel.activity1.jmi1.ActivityTracker)obj).refMofId();
					activityFilter = ACTIVITY_FILTER_TRACKER;
			} else if (obj instanceof org.opencrx.kernel.activity1.jmi1.ActivityCategory) {
				// called from ActivityCategory
				activityFilterXri = ((org.opencrx.kernel.activity1.jmi1.ActivityCategory)obj).refMofId();
				activityFilter = ACTIVITY_FILTER_CATEGORY;
			} else if (obj instanceof org.opencrx.kernel.activity1.jmi1.ActivityMilestone) {
				// called from ActivityMilestone
				activityFilterXri = ((org.opencrx.kernel.activity1.jmi1.ActivityMilestone)obj).refMofId();
				activityFilter = ACTIVITY_FILTER_MILESTONE;
			} else if ((obj instanceof org.opencrx.kernel.activity1.jmi1.Activity) || (obj instanceof org.opencrx.kernel.activity1.jmi1.ResourceAssignment)) {
					org.opencrx.kernel.activity1.jmi1.Activity activity = null;
					if (obj instanceof org.opencrx.kernel.activity1.jmi1.ResourceAssignment) {
							if (((org.opencrx.kernel.activity1.jmi1.ResourceAssignment)obj).getResource() != null) {
									resourceXri = ((org.opencrx.kernel.activity1.jmi1.ResourceAssignment)obj).getResource().refMofId();
							}
							activityXri = new Path(obj.refMofId()).getParent().getParent().toXri();
					} else {
							// called from Activity
							activityXri = ((org.opencrx.kernel.activity1.jmi1.Activity)obj).refMofId();
					}
					if (activityXri != null) {
							org.opencrx.kernel.activity1.jmi1.ActivityTracker tracker = null;
							org.opencrx.kernel.activity1.jmi1.ActivityCategory category = null;
							org.opencrx.kernel.activity1.jmi1.ActivityMilestone milestone = null;
							// hint: choose any of the assigned activity groups (preference: tracker > category > milestone), otherwise segment
							for(Iterator i = ((org.opencrx.kernel.activity1.jmi1.Activity)pm.getObjectById(new Path(activityXri))).getAssignedGroup().iterator(); i.hasNext(); ) {
									org.opencrx.kernel.activity1.jmi1.ActivityGroupAssignment ass = (org.opencrx.kernel.activity1.jmi1.ActivityGroupAssignment)i.next();
									if (ass.getActivityGroup() != null) {
											org.opencrx.kernel.activity1.jmi1.ActivityGroup ag = ass.getActivityGroup();
											if (
													ag instanceof org.opencrx.kernel.activity1.jmi1.ActivityTracker &&
													(
															((org.opencrx.kernel.activity1.jmi1.ActivityTracker)ag).isDisabled() == null ||
															!((org.opencrx.kernel.activity1.jmi1.ActivityTracker)ag).isDisabled().booleanValue()
													)
											) {
													tracker = (org.opencrx.kernel.activity1.jmi1.ActivityTracker)ag;
											} else if (
													ag instanceof org.opencrx.kernel.activity1.jmi1.ActivityCategory &&
													(
															((org.opencrx.kernel.activity1.jmi1.ActivityCategory)ag).isDisabled() == null ||
															!((org.opencrx.kernel.activity1.jmi1.ActivityCategory)ag).isDisabled().booleanValue()
													)
											) {
													category = (org.opencrx.kernel.activity1.jmi1.ActivityCategory)ag;
											} else if (
													ag instanceof org.opencrx.kernel.activity1.jmi1.ActivityMilestone &&
													(
															((org.opencrx.kernel.activity1.jmi1.ActivityMilestone)ag).isDisabled() == null ||
															!((org.opencrx.kernel.activity1.jmi1.ActivityMilestone)ag).isDisabled().booleanValue()
													)
											) {
													milestone = (org.opencrx.kernel.activity1.jmi1.ActivityMilestone)ag;
											}
									}
									if (tracker != null) {
											activityFilterXri = tracker.refMofId();
											activityFilter = ACTIVITY_FILTER_TRACKER;
									} else if (category != null) {
											activityFilterXri = category.refMofId();
											activityFilter = ACTIVITY_FILTER_CATEGORY;
									} else if (milestone != null) {
										activityFilterXri = milestone.refMofId();
										activityFilter = ACTIVITY_FILTER_MILESTONE;
									} else {
										activityFilterXri = "";
										activityFilter = ACTIVITY_FILTER_SEGMENT;
									}
							}
					} else {
							activityXri = "*";
					}
			} else if (obj instanceof org.opencrx.kernel.activity1.jmi1.Resource) {
				// called from Resource
				resourceXri = ((org.opencrx.kernel.activity1.jmi1.Resource)obj).refMofId();
				if (((org.opencrx.kernel.activity1.jmi1.Resource)obj).getContact() != null) {
						contactXri = ((org.opencrx.kernel.activity1.jmi1.Resource)obj).getContact().refMofId();
				}
			}
		} catch (Exception e) {
				new ServiceException(e).log();
		}
		if (activityFilter == null) {activityFilter = ACTIVITY_FILTER_SEGMENT;}

		// determine wheter there are ActivityTrackers with userString0 != null
		org.opencrx.kernel.activity1.cci2.ActivityTrackerQuery trackerFilter = (org.opencrx.kernel.activity1.cci2.ActivityTrackerQuery)pm.newQuery(org.opencrx.kernel.activity1.jmi1.ActivityTracker.class);
		trackerFilter.forAllDisabled().isFalse();
		trackerFilter.thereExistsUserBoolean0().isTrue();
		hasProjects = !activitySegment.getActivityTracker(trackerFilter).isEmpty();

		// determine initial setting of selector
		GregorianCalendar selectorDate = new GregorianCalendar(app.getCurrentLocale());
		selectorDate.setTimeZone(TimeZone.getTimeZone(app.getCurrentTimeZone()));
		selectorDate.setMinimalDaysInFirstWeek(4); // this conforms to DIN 1355/ISO 8601
		selectorDate.set(GregorianCalendar.DAY_OF_MONTH, 1);
		selectorDate.set(GregorianCalendar.HOUR_OF_DAY, 0);
		selectorDate.set(GregorianCalendar.MINUTE, 0);
		selectorDate.set(GregorianCalendar.SECOND, 0);
		selectorDate.set(GregorianCalendar.MILLISECOND, 0);
		selector = selectorf.format(selectorDate.getTime());
		isSelectorChange = true;
	}

	// Parameter contact
	if (contactXri == null) {contactXri = request.getParameter("contactXri");}
	org.opencrx.kernel.account1.jmi1.Contact contact = null;
	String contactXriTitle = request.getParameter("contactXri.Title") == null ? "" : request.getParameter("contactXri.Title");
	boolean showAllResources = false;
	boolean showAllResourcesOfContact = false;
	boolean isResourceChange = ((request.getParameter("isResourceChange") != null) && (request.getParameter("isResourceChange").length() > 0));
	boolean isContactChange = ((request.getParameter("isContactChange") != null) && (request.getParameter("isContactChange").length() > 0));
	try {
			if ((contactXri != null) && (contactXri.length() > 0)) {
					if (contactXri.compareTo("*") == 0) {
							showAllResources = true;
							if (!isResourceChange) {
									resourceXri = "*";
							}
					} else {
							contact = (org.opencrx.kernel.account1.jmi1.Contact)pm.getObjectById(new Path(contactXri));
					}
			} else if (obj instanceof org.opencrx.kernel.account1.jmi1.Contact) {
					contact = (org.opencrx.kernel.account1.jmi1.Contact)obj;
			} else {
					// default is current users Contact (as defined in current user's UserHome
					// get UserHome
					org.opencrx.kernel.home1.jmi1.UserHome myUserHome = org.opencrx.kernel.backend.UserHomes.getInstance().getUserHome(obj.refGetPath(), pm);
					if (myUserHome.getContact() != null) {
						contact = myUserHome.getContact();
					}
			}
	} catch (Exception e) {}

	org.opencrx.kernel.activity1.jmi1.Resource resource = null;
	if ((resourceXri == null) && (!isContactChange)) {
			resourceXri = request.getParameter("resourceXri");
			if ((resourceXri != null) && (resourceXri.length() > 0)) {
					if (resourceXri.compareTo("*") == 0) {
							showAllResourcesOfContact = true;
					} else if (isResourceChange) {
							try {
									resource = (org.opencrx.kernel.activity1.jmi1.Resource)pm.getObjectById(new Path(resourceXri));
									contact = resource.getContact();
									showAllResources = false;
							} catch (Exception e) {}
					}
			}
	}

	if (contact == null) {
			showAllResources = true;
			contactXri = "*";
			contactXriTitle = "*";
	} else {
			contactXri = contact.refMofId();
			contactXriTitle = app.getHtmlEncoder().encode(new ObjectReference(contact, app).getTitle(), false);
	}

	String projectMain = request.getParameter("projectMain") == null ? "" : request.getParameter("projectMain");

	if (activityFilterXri == null) {
			activityFilterXri = request.getParameter("activityFilterXri");
			if (activityFilterXri == null) {activityFilterXri = "";}
	}

	if (activityFilter == null) {
			activityFilter = request.getParameter("activityFilter");
			if (activityFilter == null) {activityFilter = "";}
	}

	org.opencrx.kernel.activity1.jmi1.ActivityGroup activityGroup = null;
	org.opencrx.kernel.activity1.jmi1.ActivityFilterGlobal activityFilterGlobal = null;
	try {
		if ((activityFilterXri != null) && (activityFilterXri.length() > 0)) {
				if (pm.getObjectById(new Path(activityFilterXri)) instanceof org.opencrx.kernel.activity1.jmi1.ActivityFilterGlobal) {
						activityFilterGlobal = (org.opencrx.kernel.activity1.jmi1.ActivityFilterGlobal)pm.getObjectById(new Path(activityFilterXri));
				} else {
						activityGroup = (org.opencrx.kernel.activity1.jmi1.ActivityTracker)pm.getObjectById(new Path(activityFilterXri));
				}
		}
	} catch (Exception e) {}

	if (activityXri == null) {
			activityXri = request.getParameter("activityXri")	== null ? "*" : request.getParameter("activityXri");
	}
	String recordType	= request.getParameter("recordType")	== null ? "0" : request.getParameter("recordType");	// Parameter recordType [default "0 - N/A"]
	String isBillable	= isFirstCall ? "" : request.getParameter("isBillable");
	String isReimbursable = isFirstCall ? "" : request.getParameter("isReimbursable");
	
	short priority = isFirstCall || request.getParameter("priority") == null ?
		//Activities.PRIORITY_NORMAL :
		0 : // capture ALL Activities
		Short.valueOf(request.getParameter("priority"));
	String isFullStartedAtDate	= isFirstCall ? "" : request.getParameter("isFullStartedAtDate");
	String excludeClosedActivities		 = isFirstCall ? "" : request.getParameter("excludeClosedActivities");
		String showActivityGroupNameFilter = isFirstCall ? "" : request.getParameter("showActivityGroupNameFilter");
	int activitySortOrder = 1;
	try {
			activitySortOrder = request.getParameter("activitySortOrder") != null ? Integer.parseInt(request.getParameter("activitySortOrder")) : 1;
	} catch (Exception e) {};

	String scheduledStart = request.getParameter("scheduledStart") == null ? jsCalenderf.format(new java.util.Date()) : request.getParameter("scheduledStart").trim();
	String scheduledEnd = request.getParameter("scheduledEnd") == null ? jsCalenderf.format(new java.util.Date()) : request.getParameter("scheduledEnd").trim();

	GregorianCalendar scheduledStartDate = getDateTimeAsCalendar(scheduledStart, app);
	boolean scheduledStartDateOK = scheduledStartDate != null;

	GregorianCalendar scheduledEndDate = getDateTimeAsCalendar(scheduledEnd, app);
	boolean scheduledEndDateOK = scheduledEndDate != null;

%>
<!DOCTYPE HTML PUBLIC "-//W3C//DTD HTML 4.01 Transitional//EN" "http://www.w3.org/TR/html4/loose.dtd">
<html dir="<%= texts.getDir() %>">
<head>
	<title>openCRX - Work/Expense Report</title>
	<meta http-equiv="Content-Type" content="text/html; charset=utf-8">
	<link rel="stylesheet" href="../../js/bootstrap/css/bootstrap.min.css">	
	<link rel="stylesheet" href="../../_style/colors.css">
	<link rel="stylesheet" href="../../_style/n2default.css">
	<link rel="stylesheet" href="../../_style/ssf.css">
	<link href="../../_style/calendar-small.css" rel="stylesheet">
	<link rel='shortcut icon' href='../../images/favicon.ico' />
	<script language="javascript" type="text/javascript" src="../../js/portal-all.js"></script>
	<script language="javascript" type="text/javascript" src="../../js/calendar/lang/calendar-<%= app.getCurrentLocaleAsString() %>.js"></script> <!-- calendar language -->
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

		function timeTick(hh_mm, upMins) {
			var right_now = new Date();
			var hrs = right_now.getHours();
			var mins = right_now.getMinutes();
			try {
				timeStr = hh_mm.split(":");
				hrsStr = timeStr[0];
				minsStr = timeStr[1];
			} catch (e) {}
			try {
				hrs = parseInt(hrsStr, 10);
			} catch (e) {}
			if (isNaN(hrs)) {hrs=12;}
			try {
				mins = parseInt(minsStr, 10);
				mins = parseInt(mins/15, 10)*15;
			} catch (e) {}
			if (isNaN(mins)) {mins=00;}
			mins = hrs*60 + mins + upMins;
			while (mins <			0) {mins += 24*60;}
			while (mins >= 24*60) {mins -= 24*60;}
			hrs = parseInt(mins/60, 10);
			if (hrs < 10) {
				hrsStr = "0" + hrs;
			} else {
				hrsStr = hrs;
			}
			mins -= hrs*60;
			if (mins < 10) {
				minsStr = "0" + mins;
			} else {
				minsStr = mins;
			}
			return hrsStr + ":" + minsStr;
		}

		var oldValue = "";
		function positiveDecimalsVerify(caller){
			var newValue = caller.value;
			var isOK = true;
			var i = 0;
			while ((isOK) && (i < newValue.length)) {
				var char = newValue.substring(i,i+1);
				if ((char!='.') && ((char<'0') || (char>'9'))) {isOK = false;}
				i++;
			}
			if (!isOK) {
				caller.value = oldValue;
			}
		}

	</script>

	<style type="text/css" media="all">
		fieldset {
			margin: 0px 10px 20px 0px;
			padding: 5px 0px 5px 15px;
			-moz-border-radius: 10px;
			-webkit-border-radius: 10px;
			border: 1.5px solid #DDD;
			background-color: #FAFAFA;
		}
		.small {font-size:8pt;}
		#wizMonth {
			text-align:center;
			white-space:nowrap;
		}
		input.error {background-color:red;}
		#scheduleTable, .fieldGroup {
			border-collapse: collapse;
			border-spacing:0;
			width:100%;
		}
		.fieldGroup TR TD {padding:2px 0px;}
		#scheduleTable td {
			vertical-align:top;
		}
		#scheduleTable TD.timelabel {
			background-color:#FFFE70;
			vertical-align:middle;
			border-top:1px solid #B3D7C3;
			border-bottom:1px solid #B3D7C3;
			border-left:1px solid #B3D7C3;
			white-space:nowrap;
			padding:5px;
		}
		#scheduleTable TD.time {
			background-color:#FFFE70;
			vertical-align:middle;
			border-top:1px solid #B3D7C3;
			border-right:1px solid #B3D7C3;
			border-bottom:1px solid #B3D7C3;
			white-space:nowrap;
			padding:5px;
			overflow:hidden;
		}
        .table td, .table th {
          padding: .75rem;	
        }	
		td.total {
		    border-top:1px solid black;
		    border-bottom:1px solid black;
		    font-weight:bold;
		}
		td.totalR {
		    border-top:1px solid black;
		    border-bottom:1px solid black;
		    font-weight:bold;
		    text-align:right;
		}
		TD.miniheader {font-size:7pt;}
		TD.padded_r {text-align:right;}
		TR.centered td {text-align:center;}
		TR.even td {background-color:#EEEEFF;}
		TR.match td {background-color:#FFFE70;}
		TR.break td {background-color:#FFC087;}
		TD.hidden {display:none;}
		TR.created td {font-weight:bold;}
		TD.error {color:red;font-weight:bold;}
		input.disabled {background-color:transparent;}
		input.disabled:hover {background-color:transparent;}
		.hidden {display:none;}
		.outofperiod {background-color:#F3F3F3;}
	</style>
</head>
<body>
<div id="container">
	<div id="wrap">
		<div id="scrollheader" style="height:90px;">
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
			<div id="content" style="padding:0px 0.5em 0px 0.5em;">
				<div id="aPanel">
					<div id="inspector">
						<ul class="<%= CssClass.nav %> <%= CssClass.nav_tabs %> <%= CssClass.nav_condensed %>" style="position:relative;z-index:1001;">
							<li class="<%= CssClass.nav_item %> <%= CssClass.d_print_none %>"><a class="<%= CssClass.nav_link %> <%= isWorkRecord ? CssClass.active : "" %>" href="#" onclick="$('isExpenseRecord').value='';$('reload.button').click();" href="#">Work Report</a></li>
							<li class="<%= CssClass.nav_item %> <%= CssClass.d_print_none %>"><a href="#" class="<%= CssClass.nav_link %> <%= !isWorkRecord ? CssClass.active : "" %>" onclick="$('isExpenseRecord').value='isExpenseRecord';$('reload.button').click();" href="#">Expense Report</a></li>
						</ul>
						<div id="inspContent" class="inspContent" style="z-index: 200;">
							<div id="inspPanel0" class="selected" style="padding-top: 10px;">
				<form name="<%= FORM_NAME %>" accept-charset="UTF-8" method="POST" action="<%= "../.." + request.getServletPath() %>">
					<input type="hidden" name="command" id="command" value="none"/>
					<input type="hidden" name="<%= Action.PARAMETER_REQUEST_ID %>" value="<%= requestId %>" />
					<input type="hidden" name="<%= Action.PARAMETER_OBJECTXRI %>" value="<%= objectXri %>" />
					<input type="hidden" name="isExpenseRecord" id="isExpenseRecord" value="<%= isWorkRecord ? "" : "isExpenseRecord"	%>" />
					<input type="hidden" name="hasProjects" id="hasProjects" value="<%= hasProjects ? "hasProjects" : ""	%>" />
					<input type="hidden" name="isSelectorChange" id="isSelectorChange" value="" />
					<input type="hidden" name="isContactChange" id="isContactChange" value="" />
					<input type="hidden" name="isResourceChange" id="isResourceChange" value="" />
					<input type="hidden" name="activitySortOrder" id="activitySortOrder" value="<%= activitySortOrder %>" />
					<input type="hidden" name="previousSheet" id="previousSheet" value="<%= location %>" />
					<input type="checkbox" style="display:none;" id="isFirstCall" name="isFirstCall" checked />

					<table id="scheduleTable">
						<tr>
							<td style="width:100%;">
								<fieldset>
								<table class="fieldGroup">
									<tr>
										<td class="<%= CssClass.fieldLabel %>"><span class="nw"><%= app.getTexts().getSelectAllText() %></span></td>
										<td nowrap>
<%
											boolean isManualEntry = "*".compareTo(selector) == 0;
											GregorianCalendar reportBeginOfPeriod = null;
											GregorianCalendar reportEndOfPeriod = null;
%>
											<select id="selector" name="selector" class="valueL" tabindex="<%= tabIndex++ %>" onchange="javascript:$('isSelectorChange').value='true';$('reload.button').click();" >
												<option <%= isManualEntry ? "selected" : ""	%> value="*">&mdash;&mdash;&mdash;&gt;</option>
<%
												GregorianCalendar now = new GregorianCalendar(app.getCurrentLocale());
												now.setTimeZone(TimeZone.getTimeZone(app.getCurrentTimeZone()));
												now.setMinimalDaysInFirstWeek(4); // this conforms to DIN 1355/ISO 8601
												if (isManualEntry) {
														if (scheduledStartDateOK) {
																reportBeginOfPeriod = (GregorianCalendar)scheduledStartDate.clone();
														}
														if (scheduledEndDateOK) {
																reportEndOfPeriod = (GregorianCalendar)scheduledEndDate.clone();
														}
												}

												// full months
												for (int i=-7; i <= 7; i++) {
														GregorianCalendar beginOfPeriod = new GregorianCalendar(app.getCurrentLocale());
														beginOfPeriod.setTimeZone(TimeZone.getTimeZone(app.getCurrentTimeZone()));
														beginOfPeriod.setMinimalDaysInFirstWeek(4); // this conforms to DIN 1355/ISO 8601
														beginOfPeriod.set(GregorianCalendar.DAY_OF_MONTH, 1);
														beginOfPeriod.add(GregorianCalendar.MONTH, i);
														beginOfPeriod.set(GregorianCalendar.HOUR_OF_DAY, 0);
														beginOfPeriod.set(GregorianCalendar.MINUTE, 0);
														beginOfPeriod.set(GregorianCalendar.SECOND, 0);
														beginOfPeriod.set(GregorianCalendar.MILLISECOND, 0);
														GregorianCalendar endOfPeriod = (GregorianCalendar)beginOfPeriod.clone();
														endOfPeriod.add(GregorianCalendar.MONTH, 1);
														endOfPeriod.add(GregorianCalendar.MINUTE, -1);
														String value = selectorf.format(beginOfPeriod.getTime());
														boolean selected = value.compareTo(selector) == 0;
														if (selected) {
																reportBeginOfPeriod = (GregorianCalendar)beginOfPeriod.clone();
																reportEndOfPeriod = (GregorianCalendar)endOfPeriod.clone();
																if (isSelectorChange) {
																		scheduledStartDate = (GregorianCalendar)beginOfPeriod.clone();
																		scheduledStart = jsCalenderf.format(beginOfPeriod.getTime());
																		scheduledEnd	 = jsCalenderf.format(endOfPeriod.getTime());
																		scheduledEndDate = (GregorianCalendar)endOfPeriod.clone();
																}
														}
%>
														<option <%= selected ? "selected" : ""	%> value="<%= value %>"><%= value %></option>
<%
												}
%>
											</select>
										</td>
										<td class="addon"></td>

										<td class="<%= CssClass.fieldLabel %>"><span class="nw"><%= userView.getFieldLabel(WORKANDEXPENSERECORD_CLASS, "startedAt", app.getCurrentLocaleAsIndex()) %>:</span></td>
										<td style="padding-top:2px;">
												<input type="text" class="valueL" <%= scheduledStartDateOK ? "" : ERROR_STYLE %> name="scheduledStart" id="scheduledStart" maxlength="16" tabindex="<%= tabIndex++ %>" value="<%= scheduledStartDateOK ?	jsCalenderf.format(scheduledStartDate.getTime()) : scheduledStart %>" <%= isManualEntry ? "" : "readonly style='background-color:#F3F3F3;'" %> />
										</td>
										<td class="addon">
												<a><img class="popUpButton" id="cal_trigger_scheduledStart" border="0" alt="Click to open Calendar" src="../../images/cal.gif" <%= isManualEntry ? "" : "style='display:none;'" %> /></a>
												<script language="javascript" type="text/javascript">
														Calendar.setup({
																inputField	 : "scheduledStart",
																ifFormat		 : "%d-%m-%Y %H:%M",
																timeFormat	 : "24",
																button			 : "cal_trigger_scheduledStart",
																align				: "Tr",
																singleClick	: true,
																showsTime		: true
														});
												</script>
										</td>
									</tr>

									<tr>
										<td class="<%= CssClass.fieldLabel %>"><span class="nw"></span></td>
										<td nowrap></td>
										<td class="addon"></td>

										<td class="<%= CssClass.fieldLabel %>"><span class="nw"><%= userView.getFieldLabel(WORKANDEXPENSERECORD_CLASS, "endedAt", app.getCurrentLocaleAsIndex()) %>:</span></td>
										<td style="padding-top:2px;">
												<input type="text" class="valueL" <%= scheduledEndDateOK ? "" : ERROR_STYLE %> name="scheduledEnd" id="scheduledEnd" maxlength="16" tabindex="<%= tabIndex++ %>" value="<%= scheduledEndDateOK ?	jsCalenderf.format(scheduledEndDate.getTime()) : scheduledEnd %>" <%= isManualEntry ? "" : "readonly style='background-color:#F3F3F3;'" %> />
										</td>
										<td class="addon">
												<a><img class="popUpButton" id="cal_trigger_scheduledEnd" border="0" alt="Click to open Calendar" src="../../images/cal.gif" <%= isManualEntry ? "" : "style='display:none;'" %> /></a>
												<script language="javascript" type="text/javascript">
														Calendar.setup({
																inputField	 : "scheduledEnd",
																ifFormat		 : "%d-%m-%Y %H:%M",
																timeFormat	 : "24",
																button			 : "cal_trigger_scheduledEnd",
																align				: "Tr",
																singleClick	: true,
																showsTime		: true
														});
												</script>
										</td>
									</tr>
								</table>
								</fieldset>

								<fieldset>
								<table class="fieldGroup">
									<tr>
										<td class="<%= CssClass.fieldLabel %>"><span class="nw"><%= app.getLabel(CONTACT_CLASS) %>:</span></td>
<%
										String lookupId = org.opencrx.kernel.backend.Accounts.getInstance().getUidAsString();
										Action findContactTargetObjectAction = Action.getFindObjectAction(FEATURE_CONTACT_TARGET_FINDER, lookupId);
										String accountName = app.getLabel(CONTACT_CLASS);
%>
										<td nowrap>
											<div class="autocompleterMenu">
												<ul id="<%=CssClass.ssf_nav %>" class="<%=CssClass.ssf_nav %>" onmouseover="sfinit(this);" >
													<li><a href="#"><img border="0" alt="" src="../../images/autocomplete_select.png" /></a>
														<ul onclick="this.style.left='-999em';" onmouseout="this.style.left='';">
															<li class="selected"><a href="#" onclick="javascript:navSelect(this);ac_addObject0.url= './'+getEncodedHRef(['../../ObjectInspectorServlet', 'event', '40', 'parameter', 'xri*(xri:@openmdx:org.opencrx.kernel.account1/provider/<%= providerName %>/segment/<%= segmentName %>)*referenceName*(account)*filterByType*(org:opencrx:kernel:account1:Contact)*filterByFeature*(fullName)*filterOperator*(IS_LIKE)*orderByFeature*(fullName)*position*(0)*size*(20)']);return false;"><span>&nbsp;&nbsp;&nbsp;</span><%= accountName %> / <%= userView.getFieldLabel(ACCOUNT_CLASS, "fullName", app.getCurrentLocaleAsIndex()) %></a></li>
															<li <%= userView.getFieldLabel(ACCOUNT_CLASS, "description", app.getCurrentLocaleAsIndex()) == null ? "style='display:none;" : "" %>><a href="#" onclick="javascript:navSelect(this);ac_addObject0.url= './'+getEncodedHRef(['../../ObjectInspectorServlet', 'event', '40', 'parameter', 'xri*(xri:@openmdx:org.opencrx.kernel.account1/provider/<%= providerName %>/segment/<%= segmentName %>)*referenceName*(account)*filterByType*(org:opencrx:kernel:account1:Contact)*filterByFeature*(description)*filterOperator*(IS_LIKE)*orderByFeature*(description)*position*(0)*size*(20)']);return false;"><span>&nbsp;&nbsp;&nbsp;</span><%= accountName %> / <%= userView.getFieldLabel(ACCOUNT_CLASS, "description", app.getCurrentLocaleAsIndex()) %></a></li>
															<li <%= userView.getFieldLabel(ACCOUNT_CLASS, "aliasName", app.getCurrentLocaleAsIndex()) == null ? "style='display:none;" : "" %>><a href="#" onclick="javascript:navSelect(this);ac_addObject0.url= './'+getEncodedHRef(['../../ObjectInspectorServlet', 'event', '40', 'parameter', 'xri*(xri:@openmdx:org.opencrx.kernel.account1/provider/<%= providerName %>/segment/<%= segmentName %>)*referenceName*(account)*filterByType*(org:opencrx:kernel:account1:Contact)*filterByFeature*(aliasName)*filterOperator*(IS_LIKE)*orderByFeature*(aliasName)*position*(0)*size*(20)']);return false;"><span>&nbsp;&nbsp;&nbsp;</span><%= accountName %> / <%= userView.getFieldLabel(ACCOUNT_CLASS, "aliasName", app.getCurrentLocaleAsIndex()) %></a></li>
															<li><a href="#" onclick="javascript:navSelect(this);ac_addObject0.url= './'+getEncodedHRef(['../../ObjectInspectorServlet', 'event', '40', 'parameter', 'xri*(xri:@openmdx:org.opencrx.kernel.account1/provider/<%= providerName %>/segment/<%= segmentName %>)*referenceName*(account)*filterByType*(org:opencrx:kernel:account1:Contact)*filterByFeature*(firstName)*filterOperator*(IS_LIKE)*orderByFeature*(firstName)*position*(0)*size*(20)']);return false;"><span>&nbsp;&nbsp;&nbsp;</span><%= accountName %> / <%= userView.getFieldLabel(CONTACT_CLASS, "firstName", app.getCurrentLocaleAsIndex()) %></a></li>
															<li><a href="#" onclick="javascript:navSelect(this);ac_addObject0.url= './'+getEncodedHRef(['../../ObjectInspectorServlet', 'event', '40', 'parameter', 'xri*(xri:@openmdx:org.opencrx.kernel.account1/provider/<%= providerName %>/segment/<%= segmentName %>)*referenceName*(account)*filterByType*(org:opencrx:kernel:account1:Contact)*filterByFeature*(middleName)*filterOperator*(IS_LIKE)*orderByFeature*(middleName)*position*(0)*size*(20)']);return false;"><span>&nbsp;&nbsp;&nbsp;</span><%= accountName %> / <%= userView.getFieldLabel(CONTACT_CLASS, "middleName", app.getCurrentLocaleAsIndex()) %></a></li>
															<li><a href="#" onclick="javascript:navSelect(this);ac_addObject0.url= './'+getEncodedHRef(['../../ObjectInspectorServlet', 'event', '40', 'parameter', 'xri*(xri:@openmdx:org.opencrx.kernel.account1/provider/<%= providerName %>/segment/<%= segmentName %>)*referenceName*(account)*filterByType*(org:opencrx:kernel:account1:Contact)*filterByFeature*(lastName)*filterOperator*(IS_LIKE)*orderByFeature*(lastName)*position*(0)*size*(20)']);return false;"><span>&nbsp;&nbsp;&nbsp;</span><%= accountName %> / <%= userView.getFieldLabel(CONTACT_CLASS, "lastName", app.getCurrentLocaleAsIndex()) %></a></li>
															<li <%= userView.getFieldLabel(CONTACT_CLASS, "nickName", app.getCurrentLocaleAsIndex()) == null ? "style='display:none;" : "" %>><a href="#" onclick="javascript:navSelect(this);ac_addObject0.url= './'+getEncodedHRef(['../../ObjectInspectorServlet', 'event', '40', 'parameter', 'xri*(xri:@openmdx:org.opencrx.kernel.account1/provider/<%= providerName %>/segment/<%= segmentName %>)*referenceName*(account)*filterByType*(org:opencrx:kernel:account1:Contact)*filterByFeature*(nickName)*filterOperator*(IS_LIKE)*orderByFeature*(nickName)*position*(0)*size*(20)']);return false;"><span>&nbsp;&nbsp;&nbsp;</span><%= accountName %> / <%= userView.getFieldLabel(CONTACT_CLASS, "nickName", app.getCurrentLocaleAsIndex()) %></a></li>
														</ul>
													</li>
												</ul>
											</div>
											<div class="autocompleterInput"><input type="text" class="valueL mandatory valueAC <%= contact == null ? "inputError" : "" %>" id="contactXri.Title" name="contactXri.Title" tabindex="<%= tabIndex++ %>" value="<%= contactXriTitle != null ? contactXriTitle : "" %>" /></div>
											<input type="hidden" class="valueLLocked" id="contactXri" readonly name="contactXri" value="<%= contactXri != null ? contactXri : "" %>" />
											<div class="autocomplete" id="contact.Update" style="display:none;z-index:500;"></div>
											<script type="text/javascript" language="javascript" charset="utf-8">
												function afterUpdateReload(titleField, selectedItem) {
														updateXriField(titleField, selectedItem);
														$('isContactChange').value="true";
														$('reload.button').click();
												}
												ac_addObject0 = new Ajax.Autocompleter(
													'contactXri.Title',
													'contact.Update',
													'../../ObjectInspectorServlet?event=40&parameter=xri*%28xri%3A%40openmdx%3Aorg.opencrx.kernel.account1%2Fprovider%2F<%= providerName %>%2Fsegment%2F<%= segmentName %>%29*referenceName*%28account%29*filterByType*%28org%3Aopencrx%3Akernel%3Aaccount1%3AContact%29*filterByFeature*%28fullName%29*filterOperator*%28IS_LIKE%29*orderByFeature*%28fullName%29*position*%280%29*size*%2820%29',
													{
														paramName: 'filtervalues',
														minChars: 0,
														afterUpdateElement: afterUpdateReload
													}
												);
											</script>
										</td>
										<td class="addon">
											<img class="popUpButton" border="0" alt="" src="../../images/closeInsp.gif" style="float:right;" onclick="javascript:$('contactXri').value='*';$('isContactChange').value='true';$('contactXri.Title').value='*';$('reload.button').click();" />
											<img class="popUpButton" border="0" align="bottom" alt="Click to open ObjectFinder" src="../../images/lookup.gif" onclick="OF.findObject('../../<%= findContactTargetObjectAction.getEncodedHRef() %>', $('contactXri.Title'), $('contactXri'), '<%= lookupId %>');$('isContactChange').value='true';" />
										</td>
									</tr>

									<tr>
										<td class="<%= CssClass.fieldLabel %>">
											<span class="nw"><%= app.getLabel(RESOURCE_CLASS) %>:</span>
										</td>
										<td>
<%
											boolean noResourcesFound = false;
											org.opencrx.kernel.activity1.cci2.ResourceQuery resourceFilter = (org.opencrx.kernel.activity1.cci2.ResourceQuery)pm.newQuery(org.opencrx.kernel.activity1.jmi1.Resource.class);
											if (!showAllResources) {
													resourceFilter.thereExistsContact().equalTo(contact);
											}
											resourceFilter.forAllDisabled().isFalse();
											resourceFilter.orderByName().ascending();
											resourceFilter.orderByDescription().ascending();
											List resources = activitySegment.getResource(resourceFilter);
											if (resources.isEmpty()) {
													errorMsg += "no matching resource found!<br>";
													noResourcesFound = true;
													resourceXri = "";
%>
													<select id="resourceXri" name="resourceXri" class="valueL" <%= ERROR_STYLE %> tabindex="<%= tabIndex++ %>">
														<option value="">--</option>
													</select>
<%
											} else {
%>
												<select id="resourceXri" name="resourceXri" class="valueL" tabindex="<%= tabIndex++ %>" onchange="javascript:$('isResourceChange').value='true';$('reload.button').click();" >
													<option <%= showAllResourcesOfContact ? "selected" : "" %> value="*">*</option>
<%
													for (
															Iterator i = resources.iterator();
															i.hasNext();
													) {
															org.opencrx.kernel.activity1.jmi1.Resource res = (org.opencrx.kernel.activity1.jmi1.Resource)i.next();
															String contactTitle = "--";
															try {
																	contactTitle = app.getHtmlEncoder().encode(new ObjectReference(res.getContact(), app).getTitle(), false);
															} catch (Exception e) {}
															if (((resourceXri == null) || (resourceXri.length() == 0)) && (!showAllResourcesOfContact)) {
																	resourceXri = res.refMofId();
															}
%>
															<option <%= (resourceXri != null) && (resourceXri.compareTo(res.refMofId()) == 0) ? "selected" : "" %> value="<%= res.refMofId() %>"><%= res.getName() != null ? app.getHtmlEncoder().encode(res.getName(), false) : contactTitle %><%= showAllResources ? " [" + contactTitle + "]" : "" %></option>
<%
													}
%>
												</select>
<%
											}
%>
										</td>
										<td class="addon">
												<%= noResourcesFound ? CAUTION : "" %>
										</td>
									</tr>

									<tr>
										<td class="<%= CssClass.fieldLabel %>">
											<span class="nw"><%= app.getLabel(ACTIVITYFILTER_CLASS) %>:</span>
										</td>
										<td nowrap>
<%
											Map projectNames = new TreeMap();
											Iterator projectMainIterator = null;
											Iterator activityFilterIterator = null;
											Map orderedActivityGroups = new TreeMap();

											List activitiesList = null;
											boolean openOnly = (excludeClosedActivities != null) && (excludeClosedActivities.length() > 0);
											org.opencrx.kernel.activity1.cci2.ActivityQuery activityQuery = (org.opencrx.kernel.activity1.cci2.ActivityQuery)pm.newQuery(org.opencrx.kernel.activity1.jmi1.Activity.class);
											activityQuery.forAllDisabled().isFalse();
											/*
											if (openOnly) {
												activityQuery.activityState().lessThan(
													new Short((short)20) // Status "not closed"
												);
											}
											*/
											switch (activitySortOrder) {
												case	0: activityQuery.orderByActivityNumber().ascending(); break;
												case	1: activityQuery.orderByActivityNumber().descending(); break;
												case	2: activityQuery.orderByName().ascending(); break;
												case	3: activityQuery.orderByName().descending(); break;
												default: activityQuery.orderByActivityNumber().descending(); break;
											}

											if (ACTIVITY_FILTER_SEGMENT.compareTo(activityFilter) == 0) {
												activitiesList = activitySegment.getActivity(activityQuery);
												// ensure that all activities are shown
												activityGroup = null;
												activityFilterGlobal = null;
											} else {
													int gCounter = 0;
													if (ACTIVITY_FILTER_ANYGROUP.compareTo(activityFilter) == 0) {
														// get ActivityTrackers
														org.opencrx.kernel.activity1.cci2.ActivityTrackerQuery trackerFilter = (org.opencrx.kernel.activity1.cci2.ActivityTrackerQuery)pm.newQuery(org.opencrx.kernel.activity1.jmi1.ActivityTracker.class);
														trackerFilter.forAllDisabled().isFalse();
														for(Iterator i = activitySegment.getActivityTracker(trackerFilter).iterator(); i.hasNext(); ) {
															org.opencrx.kernel.activity1.jmi1.ActivityGroup ag = (org.opencrx.kernel.activity1.jmi1.ActivityGroup)i.next();
															orderedActivityGroups.put(
																	(ag.getName() != null ? ag.getName() : "_?") + "		" + gCounter++,
																	ag
																);
														}
														// get ActivityCategories
														org.opencrx.kernel.activity1.cci2.ActivityCategoryQuery categoryFilter = (org.opencrx.kernel.activity1.cci2.ActivityCategoryQuery)pm.newQuery(org.opencrx.kernel.activity1.jmi1.ActivityCategory.class);
														categoryFilter.forAllDisabled().isFalse();
														for(Iterator i = activitySegment.getActivityCategory(categoryFilter).iterator(); i.hasNext(); ) {
															org.opencrx.kernel.activity1.jmi1.ActivityGroup ag = (org.opencrx.kernel.activity1.jmi1.ActivityGroup)i.next();
															orderedActivityGroups.put(
																	(ag.getName() != null ? ag.getName() : "_?") + "		" + gCounter++,
																	ag
																);
														}
														// get ActivityMilestones
														org.opencrx.kernel.activity1.cci2.ActivityMilestoneQuery milestoneFilter = (org.opencrx.kernel.activity1.cci2.ActivityMilestoneQuery)pm.newQuery(org.opencrx.kernel.activity1.jmi1.ActivityMilestone.class);
														milestoneFilter.forAllDisabled().isFalse();
														for(Iterator i = activitySegment.getActivityMilestone(milestoneFilter).iterator(); i.hasNext(); ) {
															org.opencrx.kernel.activity1.jmi1.ActivityGroup ag = (org.opencrx.kernel.activity1.jmi1.ActivityGroup)i.next();
															orderedActivityGroups.put(
																	(ag.getName() != null ? ag.getName() : "_?") + "		" + gCounter++,
																	ag
																);
														}
														activityFilterIterator = orderedActivityGroups.values().iterator();
													} else if (ACTIVITY_FILTER_PROJECT.compareTo(activityFilter) == 0) {
														// get projects, i.e. ActivityTrackers with userBoolean0 == true and userString0 != null
														org.opencrx.kernel.activity1.cci2.ActivityTrackerQuery trackerFilter = (org.opencrx.kernel.activity1.cci2.ActivityTrackerQuery)pm.newQuery(org.opencrx.kernel.activity1.jmi1.ActivityTracker.class);
														trackerFilter.forAllDisabled().isFalse();
														trackerFilter.thereExistsUserBoolean0().isTrue();
														trackerFilter.orderByUserString0().ascending();
														for(Iterator i = activitySegment.getActivityTracker(trackerFilter).iterator(); i.hasNext(); ) {
																org.opencrx.kernel.activity1.jmi1.ActivityTracker at = (org.opencrx.kernel.activity1.jmi1.ActivityTracker)i.next();
																if ((at.getUserString1() == null) || (at.getUserString1().length() == 0)) {
																	if ((projectMain.length() == 0) && (at.getUserString0() != null)) {
																			// set initial value of projectName
																			projectMain = at.getUserString0().trim();
																	}
																	projectNames.put(
																			(at.getUserString0() != null ? at.getUserString0().trim() : "_?"),
																			at
																		);
																}
														}
														projectMainIterator = projectNames.values().iterator(); // all distinct project names

														trackerFilter = (org.opencrx.kernel.activity1.cci2.ActivityTrackerQuery)pm.newQuery(org.opencrx.kernel.activity1.jmi1.ActivityTracker.class);
														trackerFilter.forAllDisabled().isFalse();
														trackerFilter.userString1().isNonNull();
														trackerFilter.thereExistsUserString0().equalTo(projectMain);
														for(Iterator i = activitySegment.getActivityTracker(trackerFilter).iterator(); i.hasNext(); ) {
																org.opencrx.kernel.activity1.jmi1.ActivityTracker at = (org.opencrx.kernel.activity1.jmi1.ActivityTracker)i.next();
																if ((at.getUserString1() != null) && (at.getUserString1().length() > 0)) {
																	orderedActivityGroups.put(
																			(at.getUserString1() != null ? at.getUserString1().trim() : "_?") + "		" + gCounter++,
																			at
																		);
																}
														}
														activityFilterIterator = orderedActivityGroups.values().iterator();
													} else if (ACTIVITY_FILTER_FILTERGLOBAL.compareTo(activityFilter) == 0) {
														// get ActivityFilterGlobals
														org.opencrx.kernel.activity1.cci2.ActivityFilterGlobalQuery filterGlobalFilter = (org.opencrx.kernel.activity1.cci2.ActivityFilterGlobalQuery)pm.newQuery(org.opencrx.kernel.activity1.jmi1.ActivityFilterGlobal.class);
														filterGlobalFilter.forAllDisabled().isFalse();
														for(Iterator i = activitySegment.getActivityFilter(filterGlobalFilter).iterator(); i.hasNext(); ) {
																org.opencrx.kernel.activity1.jmi1.ActivityFilterGlobal afg = (org.opencrx.kernel.activity1.jmi1.ActivityFilterGlobal)i.next();
																	orderedActivityGroups.put(
																			(afg.getName() != null ? afg.getName() : "_?") + "		" + gCounter++,
																			afg
																		);
														}
														activityFilterIterator = orderedActivityGroups.values().iterator();
													} else if (ACTIVITY_FILTER_TRACKER.compareTo(activityFilter) == 0) {
														// get ActivityTrackers
														org.opencrx.kernel.activity1.cci2.ActivityTrackerQuery trackerFilter = (org.opencrx.kernel.activity1.cci2.ActivityTrackerQuery)pm.newQuery(org.opencrx.kernel.activity1.jmi1.ActivityTracker.class);
														trackerFilter.forAllDisabled().isFalse();
														for(Iterator i = activitySegment.getActivityTracker(trackerFilter).iterator(); i.hasNext(); ) {
																org.opencrx.kernel.activity1.jmi1.ActivityGroup ag = (org.opencrx.kernel.activity1.jmi1.ActivityGroup)i.next();
																	orderedActivityGroups.put(
																			(ag.getName() != null ? ag.getName() : "_?") + "		" + gCounter++,
																			ag
																		);
														}
														activityFilterIterator = orderedActivityGroups.values().iterator();
													} else if (ACTIVITY_FILTER_CATEGORY.compareTo(activityFilter) == 0) {
														// get ActivityCategories
														org.opencrx.kernel.activity1.cci2.ActivityCategoryQuery categoryFilter = (org.opencrx.kernel.activity1.cci2.ActivityCategoryQuery)pm.newQuery(org.opencrx.kernel.activity1.jmi1.ActivityCategory.class);
														categoryFilter.forAllDisabled().isFalse();
														for(Iterator i = activitySegment.getActivityCategory(categoryFilter).iterator(); i.hasNext(); ) {
																org.opencrx.kernel.activity1.jmi1.ActivityGroup ag = (org.opencrx.kernel.activity1.jmi1.ActivityGroup)i.next();
																orderedActivityGroups.put(
																		(ag.getName() != null ? ag.getName() : "_?") + "		" + gCounter++,
																		ag
																	);
														}
														activityFilterIterator = orderedActivityGroups.values().iterator();
													} else if (ACTIVITY_FILTER_MILESTONE.compareTo(activityFilter) == 0) {
														// get ActivityMilestones
														org.opencrx.kernel.activity1.cci2.ActivityMilestoneQuery milestoneFilter = (org.opencrx.kernel.activity1.cci2.ActivityMilestoneQuery)pm.newQuery(org.opencrx.kernel.activity1.jmi1.ActivityMilestone.class);
														milestoneFilter.forAllDisabled().isFalse();
														for(Iterator i = activitySegment.getActivityMilestone(milestoneFilter).iterator(); i.hasNext(); ) {
																org.opencrx.kernel.activity1.jmi1.ActivityGroup ag = (org.opencrx.kernel.activity1.jmi1.ActivityGroup)i.next();
																orderedActivityGroups.put(
																		(ag.getName() != null ? ag.getName() : "_?") + "		" + gCounter++,
																		ag
																	);
														}
														activityFilterIterator = orderedActivityGroups.values().iterator();
													}

													tabIndex += 10;
													if (ACTIVITY_FILTER_PROJECT.compareTo(activityFilter) != 0) {
															if (activityFilterIterator == null || !activityFilterIterator.hasNext()) {
																errorMsg += "no activity groups found!<br>";
%>
																<select class="valueL" style="width:50%;float:right;" id="activityFilterXri" name="activityFilterXri" class="valueL" <%= ERROR_STYLE %> tabindex="<%= tabIndex+5 %>">
																	<option value="">--</option>
																</select>
<%
															} else {
%>
																<select class="valueL" style="width:50%;float:right;" id="activityFilterXri" name="activityFilterXri" tabindex="<%= tabIndex+5 %>" onchange="javascript:$('reload.button').click();" >
<%
																	boolean hasSelection = false;
																	if (ACTIVITY_FILTER_FILTERGLOBAL.compareTo(activityFilter) == 0) {
																			org.opencrx.kernel.activity1.jmi1.ActivityFilterGlobal firstAfg = null;
																			while (activityFilterIterator != null && activityFilterIterator.hasNext()) {
																					org.opencrx.kernel.activity1.jmi1.ActivityFilterGlobal afg = (org.opencrx.kernel.activity1.jmi1.ActivityFilterGlobal)activityFilterIterator.next();
																					boolean selected = false;
																					if ((activityFilterXri != null) && (activityFilterXri.compareTo(afg.refMofId()) == 0)) {
																							activityFilterGlobal = afg;
																							selected = true;
																							hasSelection = true;
																					}
																					if (firstAfg == null) {
																							firstAfg = afg;
																					}
%>
																					<option <%= selected ? "selected" : "" %> value="<%= afg.refMofId() %>"><%= app.getHtmlEncoder().encode(afg.getName(), false) %></option>
<%
																			}
																			if (!hasSelection) {
																					activityFilterGlobal = firstAfg; // to ensure proper location of activities
																					activityFilterXri = firstAfg.refMofId();
																			}
																	} else {
																			org.opencrx.kernel.activity1.jmi1.ActivityGroup firstAg = null;
																			while (activityFilterIterator != null && activityFilterIterator.hasNext()) {
																					org.opencrx.kernel.activity1.jmi1.ActivityGroup ag = (org.opencrx.kernel.activity1.jmi1.ActivityGroup)activityFilterIterator.next();
																					boolean selected = false;
																					if ((activityFilterXri != null) && (activityFilterXri.compareTo(ag.refMofId()) == 0)) {
																							activityGroup = ag;
																							selected = true;
																							hasSelection = true;
																					}
																					if (firstAg == null) {
																							firstAg = ag;
																					}
%>
																					<option <%= selected ? "selected" : "" %> value="<%= ag.refMofId() %>"><%= app.getHtmlEncoder().encode(ag.getName(), false) %></option>
<%
																			}
																			if (!hasSelection) {
																					activityGroup = firstAg; // to ensure proper location of activities
																					activityFilterXri = firstAg.refMofId();
																			}
																	}
%>
																</select>
<%
															}
													}
											}
%>
											<select class="valueL" style="width:<%= ACTIVITY_FILTER_SEGMENT.compareTo(activityFilter) == 0 || ACTIVITY_FILTER_PROJECT.compareTo(activityFilter) == 0 ? "100" : "49" %>%;float:left;" id="activityFilter" name="activityFilter" tabindex="<%= tabIndex++ %>" onchange="javascript:$('reload.button').click();" >
												<option <%= ACTIVITY_FILTER_SEGMENT.compareTo(activityFilter)			== 0 ? "selected" : "" %> value="<%= ACTIVITY_FILTER_SEGMENT			%>">*</option>
												<option <%= ACTIVITY_FILTER_FILTERGLOBAL.compareTo(activityFilter)== 0 ? "selected" : "" %> value="<%= ACTIVITY_FILTER_FILTERGLOBAL %>"><%= app.getLabel(ACTIVITYFILTERGLOBAL_CLASS) %></option>
												<option <%= ACTIVITY_FILTER_ANYGROUP.compareTo(activityFilter)		== 0 ? "selected" : "" %> value="<%= ACTIVITY_FILTER_ANYGROUP		 %>"><%= app.getLabel(ACTIVITYTRACKER_CLASS) %> / <%= app.getLabel(ACTIVITYCATEGORY_CLASS) %> / <%= app.getLabel(ACTIVITYMILESTONE_CLASS) %></option>
<%
												if (hasProjects) {
%>
														<option <%= ACTIVITY_FILTER_PROJECT.compareTo(activityFilter)	 == 0 ? "selected" : "" %> value="<%= ACTIVITY_FILTER_PROJECT %>"	><%= app.getLabel(ACTIVITYTRACKER_CLASS) %> [<%= userView.getFieldLabel(ACTIVITYTRACKER_CLASS, "userBoolean0", app.getCurrentLocaleAsIndex()) %>]</option>
<%
												}
%>
												<option <%= ACTIVITY_FILTER_TRACKER.compareTo(activityFilter)	 == 0 ? "selected" : "" %> value="<%= ACTIVITY_FILTER_TRACKER %>"	><%= app.getLabel(ACTIVITYTRACKER_CLASS)	 %></option>
												<option <%= ACTIVITY_FILTER_CATEGORY.compareTo(activityFilter)	== 0 ? "selected" : "" %> value="<%= ACTIVITY_FILTER_CATEGORY %>" ><%= app.getLabel(ACTIVITYCATEGORY_CLASS)	%></option>
												<option <%= ACTIVITY_FILTER_MILESTONE.compareTo(activityFilter) == 0 ? "selected" : "" %> value="<%= ACTIVITY_FILTER_MILESTONE %>"><%= app.getLabel(ACTIVITYMILESTONE_CLASS) %></option>
											</select>
										</td>
										<td class="addon"></td>
									</tr>
<%
									if (hasProjects) {
%>
										<tr <%= ACTIVITY_FILTER_PROJECT.compareTo(activityFilter) == 0 ? "" : "style='display:none;'" %>>
											<td class="<%= CssClass.fieldLabel %>">
												<span class="nw"><%= userView.getFieldLabel(ACTIVITYTRACKER_CLASS, "userBoolean0", app.getCurrentLocaleAsIndex()) %> - <%= userView.getFieldLabel(ACTIVITYTRACKER_CLASS, "userString1", app.getCurrentLocaleAsIndex()) %>:</span>
											</td>
											<td nowrap>
<%
												if (activityFilterIterator == null || !activityFilterIterator.hasNext()) {
														errorMsg += "no activity groups found!<br>";
%>
														<select class="valueL" style="width:50%;float:right;" id="activityFilterXri" name="activityFilterXri" class="valueL" <%= ERROR_STYLE %> tabindex="<%= tabIndex+5 %>">
															<option value="">--</option>
														</select>
<%
												} else {
%>
														<select class="valueL" style="width:50%;float:right;" id="activityFilterXri" name="activityFilterXri" tabindex="<%= tabIndex+5 %>" onchange="javascript:$('reload.button').click();" >
															<option <%= "*".compareTo(activityFilterXri) == 0 ? "selected" : "" %>value="*">*</option>
<%
															boolean hasSelection = "*".compareTo(activityFilterXri) == 0;
															boolean isAllProjectSubtopcis = hasSelection;
															org.opencrx.kernel.activity1.jmi1.ActivityGroup firstAg = null;
															org.opencrx.kernel.activity1.jmi1.ActivityFilterGlobal firstAfg = null;
															if (ACTIVITY_FILTER_FILTERGLOBAL.compareTo(activityFilter) == 0) {
																	while (activityFilterIterator.hasNext()) {
																		org.opencrx.kernel.activity1.jmi1.ActivityFilterGlobal afg = (org.opencrx.kernel.activity1.jmi1.ActivityFilterGlobal)activityFilterIterator.next();
																		if (isAllProjectSubtopcis) {
																			// add filtered activities of this project subtopic to the activitiesList
																			if (activitiesList == null) {
																				// init
																				activitiesList = new ArrayList();
																			}
																			for (Iterator i = afg.getFilteredActivity(activityQuery).iterator(); i.hasNext();) {
																				activitiesList.add((org.opencrx.kernel.activity1.jmi1.Activity)i.next());
																			}
																		}
																		boolean selected = false;
																		if ((activityFilterXri != null) && (activityFilterXri.compareTo(afg.refMofId()) == 0)) {
																			activityFilterGlobal = afg;
																			selected = true;
																			hasSelection = true;
																		}
																		if (firstAfg == null) {
																			firstAfg = afg;
																		}
%>
																		<option <%= (activityFilterXri != null) && (activityFilterXri.compareTo(afg.refMofId()) == 0) ? "selected" : "" %> value="<%= afg.refMofId() %>"><%= app.getHtmlEncoder().encode(afg.getName(), false) %></option>
<%
																	}
															} else {
																	while (activityFilterIterator.hasNext()) {
																		org.opencrx.kernel.activity1.jmi1.ActivityGroup ag = (org.opencrx.kernel.activity1.jmi1.ActivityGroup)activityFilterIterator.next();
																		if (isAllProjectSubtopcis) {
																			// add filtered activities of this project subtopic to the activitiesList
																			if (activitiesList == null) {
																				// init
																				activitiesList = new ArrayList();
																			}
																			for (Iterator i = ag.getFilteredActivity(activityQuery).iterator(); i.hasNext();) {
																				activitiesList.add((org.opencrx.kernel.activity1.jmi1.Activity)i.next());
																			}
																		}
																		boolean selected = false;
																		if ((activityFilterXri != null) && (activityFilterXri.compareTo(ag.refMofId()) == 0)) {
																			activityGroup = ag;
																			selected = true;
																			hasSelection = true;
																		}
																		if (firstAg == null) {
																			firstAg = ag;
																		}
%>
																		<option <%= (activityFilterXri != null) && (activityFilterXri.compareTo(ag.refMofId()) == 0) ? "selected" : "" %> value="<%= ag.refMofId() %>"><%= app.getHtmlEncoder().encode(ag.getName(), false) %></option>
<%
																	}
															}
%>
														</select>
<%
														if (!hasSelection) {
																if (ACTIVITY_FILTER_FILTERGLOBAL.compareTo(activityFilter) == 0) {
																		activityFilterGlobal = firstAfg; // to ensure proper location of activities
																		activityFilterXri = firstAfg.refMofId();
																} else {
																		activityGroup = firstAg; // to ensure proper location of activities
																		activityFilterXri = firstAg.refMofId();
																}
																// make sure that the first entry right after "*" is selected
%>
																<script language="javascript" type="text/javascript">
																	$('activityFilterXri').selectedIndex = 1;
																</script>
<%
														}
												}

												if (projectMainIterator == null || !projectMainIterator.hasNext()) {
														errorMsg += "no main topics!<br>";
%>
														<select id="projectMain" name="projectMain" class="valueL" style="width:49%;float:left;<%= ERROR_STYLEInline %>" tabindex="<%= tabIndex++ %>">
															<option value="">--</option>
														</select>
<%
												} else {
%>
														<select class="valueL" style="width:49%;float:left;" id="projectMain" name="projectMain" tabindex="<%= tabIndex++ %>" onchange="javascript:$('reload.button').click();" >
<%
															while (projectMainIterator.hasNext()) {
																org.opencrx.kernel.activity1.jmi1.ActivityTracker at = (org.opencrx.kernel.activity1.jmi1.ActivityTracker)projectMainIterator.next();
																if (at.getUserString0() != null) {
%>
																	<option <%= (projectMain != null) && (projectMain.compareTo(at.getUserString0().trim()) == 0) ? "selected" : "" %> value="<%= app.getHtmlEncoder().encode(at.getUserString0().trim(), false) %>"><%= app.getHtmlEncoder().encode(at.getUserString0().trim(), false) %></option>
<%
																}
															}
%>
														</select>
<%
												}
%>
											</td>
											<td class="addon"></td>
										</tr>
<%
									}
%>
									<tr>
										<td class="<%= CssClass.fieldLabel %>">
											<div style="float:right;">
													<img class="timeButtonL" border="0" title=">" alt="" src="../../images/filter_down_star.gif" onclick="javascript:$('activitySortOrder').value = '<%= (activitySortOrder + 1) % MAX_ACTIVITY_SORT_ORDER %>';$('reload.button').click();" />
											</div>
											<span class="nw"><%= app.getLabel(ACTIVITYSEGMENT_CLASS) %>:</span>
										</td>
										<td>
<%
											tabIndex += 10;
											if (activityGroup != null) {
												activitiesList = activityGroup.getFilteredActivity(activityQuery);
											} else if (activityFilterGlobal != null) {
												activitiesList = activityFilterGlobal.getFilteredActivity(activityQuery);
											}
											int activityCounter = 0;
											int maxToShow = MAX_ACTIVITY_SHOWN_INITIALLY;
											if (activityXri != null && "MAX".compareTo(activityXri) == 0) {
														maxToShow = MAX_ACTIVITY_SHOWN;
											}
											boolean allFilteredActivities = "*".compareTo(activityXri) == 0;
																						boolean hasActivitySelection = false;
%>
												<select id="activityXri" name="activityXri" class="valueL" tabindex="<%= tabIndex++ %>" onchange="javascript:$('reload.button').click();" >
													<option <%= allFilteredActivities ? "selected" : ""	%> value="*">*</option>
<%
													if (activitiesList != null) {
														for (
															Iterator i = activitiesList.iterator();
															i.hasNext() && (activityCounter < maxToShow);
															activityCounter++
														) {
															org.opencrx.kernel.activity1.jmi1.Activity activity = (org.opencrx.kernel.activity1.jmi1.Activity)i.next();
																																	boolean selected = (activityXri != null) && (activityXri.compareTo(activity.refMofId()) == 0);
																																	if (selected) {
																																		hasActivitySelection = true;
																																	}
%>
															<option <%= selected ? "selected" : "" %> value="<%= activity.refMofId() %>"><%= openOnly ? "" : (activity.getActivityState() < (short)20 ? "[&ensp;] " : "[X] ") %>#<%= activity.getActivityNumber() %>: <%= app.getHtmlEncoder().encode(activity.getName(), false) %></option>
<%
														}
													}
													if (activityCounter >= maxToShow) {
%>
													<option value="MAX"><%= activityCounter < MAX_ACTIVITY_SHOWN ? "&mdash;&mdash;&gt;" : "..." %></option>
<%
													}
												if (!hasActivitySelection && (activityXri != null) && (activityXri.length() > 0) && !"MAX".equalsIgnoreCase(activityXri) && !"*".equalsIgnoreCase(activityXri)) {
														// add another option to prevent loss of activity selection
														//System.out.println("activityXri = " + activityXri);
														hasActivitySelection = true;
														org.opencrx.kernel.activity1.jmi1.Activity activity = (org.opencrx.kernel.activity1.jmi1.Activity)pm.getObjectById(new Path(activityXri));
%>
														<option selected value="<%= activityXri %>"><%= openOnly ? "" : (activity.getActivityState() < (short)20 ? "[&ensp;] " : "[X] ") %>#<%= activity.getActivityNumber() %>: <%= app.getHtmlEncoder().encode(activity.getName(), false) %></option>
<%
												}
%>
											</select>
										</td>
										<td class="addon">
												<input style="display:none;" type="checkbox" id="excludeClosedActivities" name="excludeClosedActivities" title="Open Activities only" <%= (excludeClosedActivities != null) && (excludeClosedActivities.length() > 0) ? "checked" : "" %> tabindex="<%= tabIndex++ %>" value="excludeClosedActivities" onchange="javascript:$('reload.button').click();" />
										</td>
									</tr>
									<!-- isBillable -->
									<tr>
										<td class="<%= CssClass.fieldLabel %>">
											<span class="nw"><%= userView.getFieldLabel(WORKANDEXPENSERECORD_CLASS, "isBillable", app.getCurrentLocaleAsIndex()) %>:</span>
										</td>
										<td>
											<input type="checkbox" name="isBillable" <%= (isBillable != null) && (isBillable.length() > 0) ? "checked" : "" %> tabindex="<%= tabIndex++ %>" value="isBillable" onchange="javascript:$('reload.button').click();" />
										</td>
										<td class="addon"></td>
									</tr>
									<!--	isReimbursable -->
									<tr>
										<td class="<%= CssClass.fieldLabel %>">
											<span class="nw"><%= userView.getFieldLabel(WORKANDEXPENSERECORD_CLASS, "isReimbursable", app.getCurrentLocaleAsIndex()) %>:</span>
										</td>
										<td>
											<input type="checkbox" name="isReimbursable" <%= (isReimbursable != null) && (isReimbursable.length() > 0) ? "checked" : "" %> tabindex="<%= tabIndex++ %>" value="isReimbursable" onchange="javascript:$('reload.button').click();" />
										</td>
										<td class="addon"></td>
									</tr>
									<!-- showStartedAtTime -->
									<tr>
										<td class="<%= CssClass.fieldLabel %>">
											<span class="nw">Show started at time:</span>
										</td>
										<td>
											<input type="checkbox" name="isFullStartedAtDate" <%= (isFullStartedAtDate != null) && (isFullStartedAtDate.length() > 0) ? "checked" : "" %> tabindex="<%= tabIndex++ %>" value="isFullStartedAtDate" onchange="javascript:$('reload.button').click();" />
										</td>
										<td class="addon"></td>
									</tr>									
									<!--	priority -->
									<tr>
										<td class="<%= CssClass.fieldLabel %>">
											<span class="nw"><%= userView.getFieldLabel(ACTIVITY_CLASS, "priority", app.getCurrentLocaleAsIndex()) %>:</span>
										</td>
										<td>
											<select id="selector" name="priority" class="valueL" tabindex="<%= tabIndex++ %>" onchange="javascript:$('isSelectorChange').value='true';$('reload.button').click();">
<%
												Map<Short,String> priorityMap = codes.getLongTextByCode(FEATURE_PRIORITY, app.getCurrentLocaleAsIndex(), true);
												for(Map.Entry<Short,String> entry: priorityMap.entrySet()) {
													Short key = entry.getKey();
													String label = entry.getValue();
%>
													<option <%= priority == key ? "selected" : ""	%> value="<%= key %>"><%= label %></option>
<%
												}
%>
											</select>
										</td>
										<td class="addon"></td>
									</tr>
								</table>
								</fieldset>
							</td>
						</tr>
						<tr>
							<td>
								<input class="<%= CssClass.d_print_none %>" type="submit" id="EVICT_RELOAD" name="EVICT_RELOAD" tabindex="<%= tabIndex++ %>" value="<%= app.getTexts().getReloadText() %>" onclick="<%= SUBMIT_HANDLER %>" />
								<input class="<%= CssClass.d_print_none %>" type="submit" id="reload.button" name="reload.button" tabindex="<%= tabIndex++ %>" value="<%= app.getTexts().getReloadText() %>" onclick="<%= SUBMIT_HANDLER %>" style="display:none;" />
								<input class="<%= CssClass.d_print_none %>" type="submit" id="cancel.button" name="cancel.button" tabindex="<%= tabIndex++ %>" value="<%= app.getTexts().getCloseText() %>" onclick="<%= SUBMIT_HANDLER %>" /><br /><br />
								<div id="WaitIndicator" style="float:left;width:50px;height:24px;" class="wait">&nbsp;</div>
								<div id="SubmitArea" style="float:left;display:none;">
									<input class="<%= CssClass.d_print_none %>" type="submit" id="ShowReport.button" name="ShowReport.button" tabindex="<%= tabIndex++ %>" value="Show Report" onclick="<%= SUBMIT_HANDLER %>;$('WaitIndicator').style.display='block';$('SubmitArea').style.display='none';" />									
								</div>
							</td>
						</tr>
					</table>

					<br>

<!-- REPORT -->
<%
				if (actionShowReport) {
					Map selectedActivities = null;
					if (allFilteredActivities) {
						if ((ACTIVITY_FILTER_SEGMENT.compareTo(activityFilter) != 0) && (activitiesList != null)) {
							// hint: instead of adding all activities of the segment to the Map selectedActivities
							//			 is left at null; this ensures that all activities are included
							selectedActivities = new TreeMap();
							for (
									Iterator i = activitiesList.iterator();
									i.hasNext();
							) {
									org.opencrx.kernel.activity1.jmi1.Activity activity = (org.opencrx.kernel.activity1.jmi1.Activity)i.next();
									selectedActivities.put(activity.refMofId(), activity);
							}
						}
					} else if (activityXri != null && activityXri.length() > 1 && "MAX".compareTo(activityXri) != 0) {
						selectedActivities = new TreeMap();
						org.opencrx.kernel.activity1.jmi1.Activity activity = (org.opencrx.kernel.activity1.jmi1.Activity)pm.getObjectById(new Path(activityXri));
						selectedActivities.put(activity.refMofId(), activity);
					}

					Map selectedResources = null;
					boolean hasMultipleResources = true;
					if (
						!showAllResources &&
						showAllResourcesOfContact &&
						(resourceXri != null && "*".compareTo(resourceXri) == 0) &&
						contact != null
					) {
							selectedResources = new TreeMap();
							org.opencrx.kernel.activity1.cci2.ResourceQuery resFilter = (org.opencrx.kernel.activity1.cci2.ResourceQuery)pm.newQuery(org.opencrx.kernel.activity1.jmi1.Resource.class);
							resFilter.thereExistsContact().equalTo(contact);
							resFilter.forAllDisabled().isFalse();
							for (
									Iterator i = activitySegment.getResource(resFilter).iterator();
									i.hasNext();
							) {
									org.opencrx.kernel.activity1.jmi1.Resource res = (org.opencrx.kernel.activity1.jmi1.Resource)i.next();
									selectedResources.put(res.refMofId(), res);
							}
					}
					org.opencrx.kernel.activity1.jmi1.Resource selectedResource = null;
					boolean doReportCalculation = true;
					org.opencrx.kernel.activity1.cci2.WorkAndExpenseRecordQuery workAndExpenseRecordQuery = (org.opencrx.kernel.activity1.cci2.WorkAndExpenseRecordQuery)pm.newQuery(org.opencrx.kernel.activity1.jmi1.WorkAndExpenseRecord.class);
					if (reportBeginOfPeriod == null && reportEndOfPeriod == null) {
						doReportCalculation = false; // do NOT calculate work/expense report
					} else {
						if (reportBeginOfPeriod != null) {
							workAndExpenseRecordQuery.thereExistsStartedAt().greaterThanOrEqualTo(reportBeginOfPeriod.getTime());
						}
						if (reportEndOfPeriod != null) {
							workAndExpenseRecordQuery.thereExistsStartedAt().lessThanOrEqualTo(reportEndOfPeriod.getTime());
						}
					}
					if (isWorkRecord) {
						workAndExpenseRecordQuery.recordType().between(new Short((short)1), new Short((short)RECORDTYPE_WORK_MAX));
						if (uomPercent != null) {
								workAndExpenseRecordQuery.forAllQuantityUom().notEqualTo(uomPercent);
						}
					} else {
							workAndExpenseRecordQuery.recordType().greaterThan(new Short((short)RECORDTYPE_WORK_MAX));
					}
					if((isBillable != null) && (isBillable.length() > 0)) {
						workAndExpenseRecordQuery.forAllIsBillable().isTrue();
					}
					if((isReimbursable != null) && (isReimbursable.length() > 0)) {
						workAndExpenseRecordQuery.forAllIsReimbursable().isTrue();
					}
					Iterator w = null;
					ArrayList wqueue = null;
					if (
						!showAllResourcesOfContact &&
						(resourceXri != null && (resourceXri.length() > 0) && "*".compareTo(resourceXri) != 0)
					) {
						resource = (org.opencrx.kernel.activity1.jmi1.Resource)pm.getObjectById(new Path(resourceXri));
						w = resource.getWorkReportEntry(workAndExpenseRecordQuery).iterator();
						hasMultipleResources = false;
					} else {
						if (selectedActivities != null) {
							wqueue = new ArrayList<Iterator>();
							for(Iterator sa = selectedActivities.values().iterator(); sa.hasNext();) {
								try {
									org.opencrx.kernel.activity1.jmi1.Activity selectedActivity = (org.opencrx.kernel.activity1.jmi1.Activity)sa.next();
									wqueue.add(selectedActivity.getWorkReportEntry(workAndExpenseRecordQuery).iterator());
								} catch (Exception e) {
									new ServiceException(e).log();
								}
							}
						} else {		
							// note that this may result in very inefficient database queries
							w = activitySegment.getWorkReportEntry(workAndExpenseRecordQuery).iterator();
						}
					}
					
					boolean
						isProjectReporting = hasProjects &&
						(activityFilter != null) && (ACTIVITY_FILTER_PROJECT.compareTo(activityFilter) == 0) &&
						(activityFilterXri != null) && ("*".compareTo(activityFilterXri) == 0) &&
						(activityXri != null) && ("*".compareTo(activityXri) == 0) &&
						(projectMain != null);

					org.opencrx.kernel.activity1.jmi1.ActivityTracker projectTracker = null;
					Map projectPhaseList = new TreeMap();
					if (isProjectReporting) {
						// populate projectTracker and projectPhaseList
						try {
							projectTracker = (org.opencrx.kernel.activity1.jmi1.ActivityTracker)projectNames.get(projectMain);
							if (projectTracker.getUserString0() != null) {
								// get phases of project
								org.opencrx.kernel.activity1.cci2.ActivityTrackerQuery trackerFilter = (org.opencrx.kernel.activity1.cci2.ActivityTrackerQuery)pm.newQuery(org.opencrx.kernel.activity1.jmi1.ActivityTracker.class);
								trackerFilter.forAllDisabled().isFalse();
								trackerFilter.userString1().isNonNull();
								trackerFilter.thereExistsUserString0().equalTo(projectMain);
								trackerFilter.orderByName().ascending();
								for(Iterator i = activitySegment.getActivityTracker(trackerFilter).iterator(); i.hasNext(); ) {
									org.opencrx.kernel.activity1.jmi1.ActivityTracker at = (org.opencrx.kernel.activity1.jmi1.ActivityTracker)i.next();
									if ((at.getUserString1() != null) && (at.getUserString1().length() > 0)) {
											projectPhaseList.put((at.getName() != null ? at.getName().toUpperCase() : "") + GAP_BEFORE_XRI + at.refMofId(), at);
									}
								}
							}

						} catch (Exception e) {
								new ServiceException(e).log();
						}
					}
					isProjectReporting = isProjectReporting && (projectTracker != null) && (!projectPhaseList.isEmpty());

					// init totalizers
					Map totals = new TreeMap();	// (currency, double)
					Map totalsBillable = new TreeMap();	// (currency, double)
					Map totalsReimbursable = new TreeMap();	// (currency, double)

					Map activities = new TreeMap();	// (activityNumber, XRI)
					Map totalsPerActivity = new TreeMap();	// (activityNumber_currency, double)

					Map activityGroups = new TreeMap();	// (name	 XRI, XRI)
					Map totalsPerActivityGroup	= new TreeMap();	// (XRI_currency, double)

					Map totalsProject = new TreeMap();	// (currency, double)
					Map projectPhases = new TreeMap();	// (name	 XRI, XRI)
					Map totalsPerProjectPhase = new TreeMap();	// (XRI_currency, double)

					Map totalsReportingLines = new TreeMap();	// (currency, double)
					Map reportingLines = new TreeMap();	// (sortKey, KEY)
					Map totalsPerProjectReportLine = new TreeMap();	// (KEY[activityGroupXRI, activityXRI, resourceXRI, redordType, rate]_currency, double)

					Map dayLoads = new TreeMap();							// (KEY[YYYYMMDD, resourceXRI], double)
					Map dayPercentageTotals = new TreeMap();	// (KEY[YYYYMMDD, resourceXRI], double) [should be equal to 100%]
					Map activityTotals = new TreeMap();				// (KEY[activityXRI, resourceXRI], double)
					Map resourceTotals = new TreeMap();				// (KEY[resourceXRI], double)
					Map dayErrorMessages = new TreeMap();			// (KEY[YYYYMMDD], String)

					final String KEY_SPLITTER = "-;;-";

					Map totalsPerWeekDay = new TreeMap();	// (yyyyNNDD_currency, double)	[NN = WEEK_OF_YEAR, EE = DAY_OF_WEEK]

					List recordsWithoutAssignedActivityGroup = new ArrayList(); // list of all WorkAndExpenseRecords that are not assigned to any ActivityGroup

					String timeKey = " hh:mm";
					totals.put(timeKey, (Double)0.0);
					totalsProject.put(timeKey, (Double)0.0);
					totalsReportingLines.put(timeKey, (Double)0.0);
					totalsBillable.put(timeKey, (Double)0.0);
					totalsReimbursable.put(timeKey, (Double)0.0);
					boolean totalsError = false;

					java.util.Date earliestDate = null;
					java.util.Date latestDate = null;

					Map workReportEntries = new TreeMap(); // (startedAt[YYYYMMDD]-counter[00000], WorkEndExpenseRecord)
					int counter = 0;
					int wqueueIdx = -1;

					while (doReportCalculation && ((w != null && w.hasNext()) || wqueue != null)) {
						org.opencrx.kernel.activity1.jmi1.WorkAndExpenseRecord workAndExpenseRecord = null;
						if (w != null && w.hasNext()) {
							workAndExpenseRecord = (org.opencrx.kernel.activity1.jmi1.WorkAndExpenseRecord)w.next();
						} else {
							try {
								wqueueIdx++;
								if (wqueue != null && wqueue.size() > wqueueIdx && wqueue.get(wqueueIdx) != null) {
									// get next iterator
									w = (Iterator)wqueue.get(wqueueIdx);
									if (w.hasNext()) {
										workAndExpenseRecord = (org.opencrx.kernel.activity1.jmi1.WorkAndExpenseRecord)w.next();
									} else {
										continue;
									}
								} else {
									wqueue = null;
									continue;
								}
							} catch (Exception e) {
								new ServiceException(e).log();
							}
						}
						if(
							workAndExpenseRecord.getActivity() != null &&
							(workAndExpenseRecord.getActivity().getPriority() >= priority)
						) {
							org.opencrx.kernel.activity1.jmi1.Activity activity = workAndExpenseRecord.getActivity();
							String activityNumber = null;
							boolean hasAssignedActivityGroup = false;
							boolean allocatedToProjectPhase = false;
							try {
								activityNumber = activity.getActivityNumber() == null ?
									"-" :
										activity.getActivityNumber();
							} catch (Exception e) {
								activityNumber = "-";
							}
							String resourceKey = "*";
							try {
									if (workAndExpenseRecord != null && workAndExpenseRecord.getResource() != null) {
											resourceKey = workAndExpenseRecord.getResource().refMofId();
									}
							} catch (Exception e) {}
							if (
								((selectedActivities == null) || (selectedActivities.containsKey(activity.refMofId()))) &&
								((selectedResources	== null) || (selectedResources.containsKey(resourceKey)))
							) {
								String sortKey =
									(workAndExpenseRecord.getStartedAt() != null ? dtsortf.format(workAndExpenseRecord.getStartedAt()) : "yyyyMMddHHmmss") + "-" +
									activityNumber + "-" +
									(workAndExpenseRecord.getResource() != null ? (new ObjectReference(resource, app)).getTitle() : "-") +
									(workAndExpenseRecord.getName() != null ? workAndExpenseRecord.getName() : "-") +
									formatter.format(counter++);
								workReportEntries.put(sortKey, workAndExpenseRecord);
								//System.out.println("added workRecord: " + dtsortf.format(workAndExpenseRecord.getStartedAt()) + "-" + formatter.format(counter++));

								if (earliestDate == null || (workAndExpenseRecord.getStartedAt() != null && earliestDate.compareTo(workAndExpenseRecord.getStartedAt()) > 0)) {
									earliestDate = workAndExpenseRecord.getStartedAt();
								}
								if (latestDate == null || (workAndExpenseRecord.getStartedAt() != null && latestDate.compareTo(workAndExpenseRecord.getStartedAt()) < 0)) {
									latestDate = workAndExpenseRecord.getStartedAt();
								}
								double recordTotal = 0.0;
								double timeTotal = 0.0;
								boolean quantityError = false;
								try {
									if (workAndExpenseRecord.getQuantity() != null) {
										timeTotal = workAndExpenseRecord.getQuantity().doubleValue();
										if (workAndExpenseRecord.getRate() != null) {
											recordTotal = workAndExpenseRecord.getQuantity().doubleValue() * workAndExpenseRecord.getRate().doubleValue();
										} else {
											totalsError = true;
										}
									} else {
										quantityError = true;
									}
								} catch (Exception e) {
									quantityError = true;
									totalsError = true;
								}
								if (workAndExpenseRecord.getBillingCurrency() == 0) {
									quantityError = true;
									totalsError = true;
								}
								String currency = DEFAULT_CURRENCY;
								try {
									currency = (String)(codes.getShortText(FEATURE_BILLING_CURRENCY, app.getCurrentLocaleAsIndex(), true, true).get(new Short(workAndExpenseRecord.getBillingCurrency())));
								} catch (Exception e) {};
								if (totals.get(currency) == null) {
									// init this currency
									totals.put(currency, (Double)0.0);
									totalsBillable.put(currency, (Double)0.0);
									totalsReimbursable.put(currency, (Double)0.0);
								}
								String activityKey = null; // activityNumber
								String activityTimeKey = null;
								try {
									try {
										activities.put(activityNumber, activity.refMofId());
									} catch (Exception ea) {
										// probably a work record with missing activity
										new ServiceException(ea).log();
									}
									activityKey = activityNumber + "_" + currency;
									activityTimeKey = activityNumber + "_" + timeKey;
								} catch (Exception e) {
									totalsError = true;
									activityKey = "?_" + currency;
									activityTimeKey = "?_" + timeKey;
								}
								if (totalsPerActivity.get(activityKey) == null) {
									totalsPerActivity.put(activityKey, (Double)0.0);
								}
								if (totalsPerActivity.get(activityTimeKey) == null) {
									totalsPerActivity.put(activityTimeKey, (Double)0.0);
								}
								String activityGroupKey = "?_" + currency; // name_XRI
								String activityGroupTimeKey = "?_" + timeKey;
								org.opencrx.kernel.activity1.cci2.ActivityGroupAssignmentQuery activityGroupAssignmentFilter = (org.opencrx.kernel.activity1.cci2.ActivityGroupAssignmentQuery)pm.newQuery(org.opencrx.kernel.activity1.jmi1.ActivityGroupAssignment.class);
								activityGroupAssignmentFilter.orderByCreatedAt().ascending();
								for(Iterator i = activity.getAssignedGroup(activityGroupAssignmentFilter).iterator(); i.hasNext(); ) {
									org.opencrx.kernel.activity1.jmi1.ActivityGroupAssignment agas = (org.opencrx.kernel.activity1.jmi1.ActivityGroupAssignment)i.next();
									org.opencrx.kernel.activity1.jmi1.ActivityGroup ag = null;
									try {
											ag = agas.getActivityGroup();
									} catch (Exception e) {}
									if (ag != null) {
										hasAssignedActivityGroup = true;
										if (
											isProjectReporting &&
											!allocatedToProjectPhase &&
											projectPhaseList.values().contains(ag)
										) {
											String projectPhaseKey = "?_" + currency; // name_XRI
											String projectPhaseTimeKey = "?_" + timeKey;
											String reportingLineKEY = "@";
											String reportLineKey =	"?_" + currency;
											String reportLineTimeKey = "?_" + timeKey;
											try {
												projectPhases.put((ag.getName() != null ? ag.getName().toUpperCase() : "") + GAP_BEFORE_XRI + ag.refMofId(), ag.refMofId());
												projectPhaseKey = (ag.getName() != null ? ag.getName().toUpperCase() : "") + GAP_BEFORE_XRI + ag.refMofId() + "_" + currency;
												projectPhaseTimeKey = (ag.getName() != null ? ag.getName().toUpperCase() : "") + GAP_BEFORE_XRI + ag.refMofId() + "_" + timeKey;

												reportingLineKEY =
													ag.refMofId() + KEY_SPLITTER +
													activity.refMofId() + KEY_SPLITTER +
													(workAndExpenseRecord.getResource() != null ? workAndExpenseRecord.getResource().refMofId() : "?") + KEY_SPLITTER +
													formatter.format(workAndExpenseRecord.getRecordType()) + KEY_SPLITTER +
													(workAndExpenseRecord.getRate() != null ? workAndExpenseRecord.getRate() : "0");
												String rSortKey =
													(ag.getName() != null ? ag.getName().toUpperCase() : "") + activityNumber +
													(workAndExpenseRecord.getResource() != null ? (new ObjectReference(workAndExpenseRecord.getResource(), app)).getTitle() : "?") +
													formatter.format(workAndExpenseRecord.getRecordType());
												reportingLines.put(rSortKey, reportingLineKEY);
												reportLineKey =	reportingLineKEY + KEY_SPLITTER +	"_" + currency;
												reportLineTimeKey =	reportingLineKEY + KEY_SPLITTER +	"_" + timeKey;
												allocatedToProjectPhase = true;
											} catch (Exception ea) {
												totalsError = true;
												projectPhaseKey = "?_" + currency;
												projectPhaseTimeKey = "?_" + timeKey;
												reportLineKey =	"?_" + currency;
												reportLineTimeKey = "?_" + timeKey;
											}
											if (totalsPerProjectPhase.get(projectPhaseKey) == null) {
												totalsPerProjectPhase.put(projectPhaseKey, (Double)0.0);
											}
											if (totalsPerProjectPhase.get(projectPhaseTimeKey) == null) {
												totalsPerProjectPhase.put(projectPhaseTimeKey, (Double)0.0);
											}
											if (totalsPerProjectReportLine.get(reportLineKey) == null) {
												totalsPerProjectReportLine.put(reportLineKey, (Double)0.0);
											}
											if (totalsPerProjectReportLine.get(reportLineTimeKey) == null) {
												totalsPerProjectReportLine.put(reportLineTimeKey, (Double)0.0);
											}
											if (isWorkRecord && (timeTotal != 0.0)) {
												totalsPerProjectPhase.put(projectPhaseTimeKey, ((Double)totalsPerProjectPhase.get(projectPhaseTimeKey)) + timeTotal);
												totalsPerProjectReportLine.put(reportLineTimeKey, ((Double)totalsPerProjectReportLine.get(reportLineTimeKey)) + timeTotal);
											}
											totalsPerProjectPhase.put(projectPhaseKey, ((Double)totalsPerProjectPhase.get(projectPhaseKey)) + recordTotal);
											totalsPerProjectReportLine.put(reportLineKey, ((Double)totalsPerProjectReportLine.get(reportLineKey)) + recordTotal);

											if (totalsProject.get(currency) == null) {
												totalsProject.put(currency, (Double)0.0);
											}
											if (isWorkRecord && (timeTotal != 0.0)) {
												totalsProject.put(timeKey, ((Double)totalsProject.get(timeKey)) + timeTotal);
											}
											totalsProject.put(currency, ((Double)totalsProject.get(currency)) + recordTotal);

											if (totalsReportingLines.get(currency) == null) {
												totalsReportingLines.put(currency, (Double)0.0);
											}
											if (isWorkRecord && (timeTotal != 0.0)) {
												totalsReportingLines.put(timeKey, ((Double)totalsReportingLines.get(timeKey)) + timeTotal);
											}
											totalsReportingLines.put(currency, ((Double)totalsReportingLines.get(currency)) + recordTotal);
										}
										try {
											try {
												activityGroups.put((ag.getName() != null ? ag.getName().toUpperCase() : "") + GAP_BEFORE_XRI + ag.refMofId(), ag.refMofId());
											} catch (Exception ea) {
													new ServiceException(ea).log();
											}
											activityGroupKey = (ag.getName() != null ? ag.getName().toUpperCase() : "") + GAP_BEFORE_XRI + ag.refMofId() + "_" + currency;
											activityGroupTimeKey = (ag.getName() != null ? ag.getName().toUpperCase() : "") + GAP_BEFORE_XRI + ag.refMofId() + "_" + timeKey;
										} catch (Exception e) {
											totalsError = true;
											activityGroupKey = "?_" + currency;
											activityGroupTimeKey = "?_" + timeKey;
										}
										if (totalsPerActivityGroup.get(activityGroupKey) == null) {
											totalsPerActivityGroup.put(activityGroupKey, (Double)0.0);
										}
										if (totalsPerActivityGroup.get(activityGroupTimeKey) == null) {
											totalsPerActivityGroup.put(activityGroupTimeKey, (Double)0.0);
										}
										if (isWorkRecord && (timeTotal != 0.0)) {
											totalsPerActivityGroup.put(activityGroupTimeKey, ((Double)totalsPerActivityGroup.get(activityGroupTimeKey)) + timeTotal);
										}
										totalsPerActivityGroup.put(activityGroupKey, ((Double)totalsPerActivityGroup.get(activityGroupKey)) + recordTotal);
									}
								}

								String WWDDKey = null;		// YYYYWWDD where WW week of year and DD day of week
								String WWDDsumKey = null; // YYYYWW99 (sum of week)
								String WWDDTimeKey = null;
								String WWDDsumTimeKey = null;
								try {
									GregorianCalendar cal = new GregorianCalendar(app.getCurrentLocale());
									cal.setTimeZone(TimeZone.getTimeZone(app.getCurrentTimeZone()));
									cal.setMinimalDaysInFirstWeek(4); // this conforms to DIN 1355/ISO 8601
									cal.setTime(workAndExpenseRecord.getStartedAt());
									WWDDKey =
										yyyyf.format(cal.getTime()) +
										formatter2.format(cal.get(GregorianCalendar.WEEK_OF_YEAR)) +
										formatter2.format(cal.get(GregorianCalendar.DAY_OF_WEEK) % 7) + "_" + currency;
									WWDDsumKey =
										yyyyf.format(cal.getTime()) +
										formatter2.format(cal.get(GregorianCalendar.WEEK_OF_YEAR)) +
										"99_" + currency;
									WWDDTimeKey =
										yyyyf.format(cal.getTime()) +
										formatter2.format(cal.get(GregorianCalendar.WEEK_OF_YEAR)) +
										formatter2.format(cal.get(GregorianCalendar.DAY_OF_WEEK) % 7) + "_" + timeKey;
									WWDDsumTimeKey =
										yyyyf.format(cal.getTime()) +
										formatter2.format(cal.get(GregorianCalendar.WEEK_OF_YEAR)) +
										"99_" + timeKey;
								} catch (Exception e) {
									totalsError = true;
									WWDDKey = "?_" + currency;
									WWDDsumKey = "9_" + currency;
									WWDDTimeKey = "?_" + timeKey;
									WWDDsumTimeKey = "9_" + timeKey;
								}
								if (totalsPerWeekDay.get(WWDDKey) == null) {
									totalsPerWeekDay.put(WWDDKey, (Double)0.0);
								}
								if (totalsPerWeekDay.get(WWDDsumKey) == null) {
									totalsPerWeekDay.put(WWDDsumKey, (Double)0.0);
								}
								if (totalsPerWeekDay.get(WWDDTimeKey) == null) {
									totalsPerWeekDay.put(WWDDTimeKey, (Double)0.0);
								}
								if (totalsPerWeekDay.get(WWDDsumTimeKey) == null) {
									totalsPerWeekDay.put(WWDDsumTimeKey, (Double)0.0);
								}

								// add totals of current record to overall totals
								totals.put(currency, ((Double)totals.get(currency)) + recordTotal);
								if (workAndExpenseRecord.isBillable() != null && workAndExpenseRecord.isBillable().booleanValue()) {
									totalsBillable.put(currency, ((Double)totalsBillable.get(currency)) + recordTotal);
								}
								if (workAndExpenseRecord.isReimbursable() != null && workAndExpenseRecord.isReimbursable().booleanValue()) {
									totalsReimbursable.put(currency, ((Double)totalsReimbursable.get(currency)) + recordTotal);
								}
								if (isWorkRecord && (timeTotal != 0.0)) {
									totals.put(timeKey, ((Double)totals.get(timeKey)) + timeTotal);
									if (workAndExpenseRecord.isBillable() != null && workAndExpenseRecord.isBillable().booleanValue()) {
										totalsBillable.put(timeKey, ((Double)totalsBillable.get(timeKey)) + timeTotal);
									}
									if (workAndExpenseRecord.isReimbursable() != null && workAndExpenseRecord.isReimbursable().booleanValue()) {
										totalsReimbursable.put(timeKey, ((Double)totalsReimbursable.get(timeKey)) + timeTotal);
									}
									totalsPerActivity.put(activityTimeKey, ((Double)totalsPerActivity.get(activityTimeKey)) + timeTotal);
									totalsPerWeekDay.put(WWDDTimeKey, ((Double)totalsPerWeekDay.get(WWDDTimeKey)) + timeTotal);
									totalsPerWeekDay.put(WWDDsumTimeKey, ((Double)totalsPerWeekDay.get(WWDDsumTimeKey)) + timeTotal);
								}
								totalsPerActivity.put(activityKey, ((Double)totalsPerActivity.get(activityKey)) + recordTotal);
								totalsPerWeekDay.put(WWDDKey, ((Double)totalsPerWeekDay.get(WWDDKey)) + recordTotal);
								totalsPerWeekDay.put(WWDDsumKey, ((Double)totalsPerWeekDay.get(WWDDsumKey)) + recordTotal);
							}
							if (!hasAssignedActivityGroup || (isProjectReporting && !allocatedToProjectPhase)) {
								totalsError = true;
								recordsWithoutAssignedActivityGroup.add(workAndExpenseRecord);
							}
						}
					}
					String contactTitle = "--";
					try {
						if (resource != null) {
							contactTitle = app.getHtmlEncoder().encode(new ObjectReference(resource.getContact(), app).getTitle(), false);
						}
					} catch (Exception e) {}

					String resHref = "";
					if (resource != null) {
						Action action = new Action(
								SelectObjectAction.EVENT_ID,
								new Action.Parameter[]{
										new Action.Parameter(Action.PARAMETER_OBJECTXRI, resource.refMofId())
								},
								"",
								true // enabled
							);
						resHref = "../../" + action.getEncodedHRef();
						resHref = "<a href='" + resHref + "' target='_blank'>" + app.getHtmlEncoder().encode(new ObjectReference(resource, app).getTitle(), false) + "</a>";
					}

					String contactHref = "";
					if (contact != null) {
						Action action = new Action(
								SelectObjectAction.EVENT_ID,
								new Action.Parameter[]{
										new Action.Parameter(Action.PARAMETER_OBJECTXRI, contact.refMofId())
								},
								"",
								true // enabled
							);
						contactHref = "../../" + action.getEncodedHRef();
						contactHref = "<a href='" + contactHref + "' target='_blank'>" + app.getHtmlEncoder().encode(new ObjectReference(contact, app).getTitle(), false) + "</a>";
					}

					String activityGroupHref = "*";
					if (activityGroup != null) {
						Action action = new Action(
								SelectObjectAction.EVENT_ID,
								new Action.Parameter[]{
										new Action.Parameter(Action.PARAMETER_OBJECTXRI, activityGroup.refMofId())
								},
								"",
								true // enabled
							);
						activityGroupHref = "../../" + action.getEncodedHRef();
						activityGroupHref = "<a href='" + activityGroupHref + "' target='_blank'>" + app.getHtmlEncoder().encode(new ObjectReference(activityGroup, app).getTitle(), false) + "</a>";
					}

					String actHref = "*";
					org.opencrx.kernel.activity1.jmi1.Activity act = null;
					if (activityXri != null &&	"*".compareTo(activityXri) != 0) {
							try {
									act = (org.opencrx.kernel.activity1.jmi1.Activity)pm.getObjectById(new Path(activityXri));
							} catch (Exception e) {}
					}
					if (act != null) {
						Action action = new Action(
								SelectObjectAction.EVENT_ID,
								new Action.Parameter[]{
										new Action.Parameter(Action.PARAMETER_OBJECTXRI, act.refMofId())
								},
								"",
								true // enabled
							);
						actHref = "../../" + action.getEncodedHRef();
						actHref = "<a href='" + actHref + "' target='_blank'>#" + app.getHtmlEncoder().encode(new ObjectReference(act, app).getTitle(), false) + "</a>";
					}

					Action downloadAction =	new Action(
						Action.EVENT_DOWNLOAD_FROM_LOCATION,
						new Action.Parameter[]{
							new Action.Parameter(Action.PARAMETER_LOCATION, location),
							new Action.Parameter(Action.PARAMETER_NAME, sheetName + ".xls"),
							new Action.Parameter(Action.PARAMETER_MIME_TYPE, "application/vnd.ms-excel")
						},
						app.getTexts().getClickToDownloadText() + " " + sheetName,
						true
					);

%>
					<div class="<%= CssClass.d_print_none%>">
						<a href="<%= request.getContextPath() %>/<%= downloadAction.getEncodedHRef(requestId) %>"><%= SPREADSHEET %></a>
					</div>
					<hr>
					<div style="clear:left;"></div>
<%
/*---------------------------------------------------------------------------------------------------------------------
 N O T E :	 It is assumed that ALL work records (i.e. WorkAndExpenseRecords with recordType <= 99 have hours as UOM!!!
						 unless they are marked as workRecordInPercent with quantityUom = 'Percent'
---------------------------------------------------------------------------------------------------------------------*/

					if (doReportCalculation) {

							String[] labels = new String[] {
									app.getLabel(WORKANDEXPENSERECORD_CLASS),
									app.getLabel(CONTACT_CLASS),
									app.getLabel(RESOURCE_CLASS),
									(isProjectReporting
											? userView.getFieldLabel(ACTIVITYTRACKER_CLASS, "userString0", app.getCurrentLocaleAsIndex())
											: app.getLabel(ACTIVITYFILTER_CLASS)
									)
							};

							String[] values = new String[] {
									(reportBeginOfPeriod != null ? datef.format(reportBeginOfPeriod.getTime()) : "--") + " - " + (reportEndOfPeriod != null ? datef.format(reportEndOfPeriod.getTime()) : "--"),
									(contact != null ? app.getHtmlEncoder().encode(contact.getLastName(), false) + ", " + app.getHtmlEncoder().encode(contact.getFirstName(), false) : "*"),
									(resource != null ? app.getHtmlEncoder().encode(resource.getName(), false) : "*"),
									(isProjectReporting
											? (projectMain != null ? app.getHtmlEncoder().encode(projectMain, false) : "--")
											: ((activityGroup != null ? app.getHtmlEncoder().encode(activityGroup.getName(), false) : "*") + " (" + (act != null ? ("#" + act.getActivityNumber() + " " + app.getHtmlEncoder().encode(act.getName(), false)) : "*") + ")")
									)
							};

							HSSFSheet sheetProject = null;
							HSSFSheet sheetWeeks = null;
							HSSFSheet sheetActivityGroups = null;
							HSSFSheet sheetResources = null;
							HSSFSheet sheetRecords = addSheet(wb, "Records",		true,	labels, values, 1);
							sheetWeeks = addSheet(wb, "Calendar",		false, labels, values, 0);
							HSSFSheet sheetActivities = addSheet(wb, "Activities",		 false, labels, values, 0);
							if(hasMultipleResources) {
								sheetResources = addSheet(wb, "Resources",		false, labels, values, 0);
							}
							sheetActivityGroups = addSheet(wb, "ActivityGroups", false, labels, values, 0);
							HSSFRow row = null;
							HSSFCell cell = null;
							HSSFCell lastSumCell = null;
							HSSFCell preLastSumCell = null;
							HSSFCell lastNumDayCell = null;
							HSSFCell preLastNumDayCell = null;
							HSSFCell resourceLineCell = null;
							short nRow = 0;
							short nCell = 0;
							boolean showFullStartedAtDate = (isFullStartedAtDate != null) && (isFullStartedAtDate.length() > 0);
							sheetRecords.setColumnWidth((short)0, (short)1200); //A
							sheetRecords.setColumnWidth((short)1, (short)4500); //B - startedAt
							sheetRecords.setColumnWidth((short)2, (short)1000); //C
							sheetRecords.setColumnWidth((short)3, (short)4500); //D - endedAt
							sheetRecords.setColumnWidth((short)4, (short) 400); //E
							sheetRecords.setColumnWidth((short)5, (short)9000); //F - activity
							sheetRecords.setColumnWidth((short)6, (short)3000); //G - name
							sheetRecords.setColumnWidth((short)7, (short)4000); //H - description
							sheetRecords.setColumnWidth((short)8, (short)4000); //I - resource
							sheetRecords.setColumnWidth((short)9, (short)0);    //J - reportingAccount
							sheetRecords.setColumnWidth((short)10, (short)4000);//K

							nRow = REPORT_STARTING_ROW;
							row = sheetRecords.createRow(nRow++);
							nCell = 0;
							cell = row.createCell(nCell++);
							cell = row.createCell(nCell++);	cell.setCellValue(userView.getFieldLabel(WORKANDEXPENSERECORD_CLASS, "startedAt", app.getCurrentLocaleAsIndex())); cell.setCellStyle(rightAlignStyle);
							cell = row.createCell(nCell++);
							cell = row.createCell(nCell++); cell.setCellValue(userView.getFieldLabel(WORKANDEXPENSERECORD_CLASS, "endedAt", app.getCurrentLocaleAsIndex())); cell.setCellStyle(rightAlignStyle);
							cell = row.createCell(nCell++);
							cell = row.createCell(nCell++);	cell.setCellValue(userView.getFieldLabel(WORKANDEXPENSERECORD_CLASS, "activity", app.getCurrentLocaleAsIndex()));
							cell = row.createCell(nCell++);	cell.setCellValue(userView.getFieldLabel(WORKANDEXPENSERECORD_CLASS, "name", app.getCurrentLocaleAsIndex()));
							cell = row.createCell(nCell++);	cell.setCellValue(userView.getFieldLabel(WORKANDEXPENSERECORD_CLASS, "description", app.getCurrentLocaleAsIndex()));
							cell = row.createCell(nCell++);	cell.setCellValue(userView.getFieldLabel(WORKANDEXPENSERECORD_CLASS, "resource", app.getCurrentLocaleAsIndex()));
							cell = row.createCell(nCell++);	cell.setCellValue(userView.getFieldLabel(ACTIVITY_CLASS, "reportingAccount", app.getCurrentLocaleAsIndex()));
							if (isWorkRecord) {
								cell = row.createCell(nCell++);	cell.setCellValue("hh:mm"); cell.setCellStyle(timeStyle);
								sheetRecords.setColumnWidth((short)10, (short)1800);// K
								sheetRecords.setColumnWidth((short)11, (short)1800);// L
								sheetRecords.setColumnWidth((short)12, (short)1200);// M
								sheetRecords.setColumnWidth((short)13, (short)3000);// N
								sheetRecords.setColumnWidth((short)14, (short)3000);// O
								sheetRecords.setColumnWidth((short)15, (short)3000);// P
								sheetRecords.setColumnWidth((short)16, (short)1000);// Q
								sheetRecords.setColumnWidth((short)17, (short)4000);// R
								sheetRecords.setColumnWidth((short)18, (short)6000);// S
							} else {
								cell = row.createCell(nCell++);	cell.setCellValue(userView.getFieldLabel(WORKANDEXPENSERECORD_CLASS, "quantity", app.getCurrentLocaleAsIndex())); cell.setCellStyle(rightAlignStyle);
								cell = row.createCell(nCell++);
								cell = row.createCell(nCell++);	cell.setCellValue(userView.getFieldLabel(WORKANDEXPENSERECORD_CLASS, "recordType", app.getCurrentLocaleAsIndex()));
								cell = row.createCell(nCell++);	cell.setCellValue(userView.getFieldLabel(WORKANDEXPENSERECORD_CLASS, "quantityUom", app.getCurrentLocaleAsIndex()));
								sheetRecords.setColumnWidth((short)10, (short)2000);
								sheetRecords.setColumnWidth((short)11, (short)1000);
								sheetRecords.setColumnWidth((short)12, (short)3000);
								sheetRecords.setColumnWidth((short)13, (short)3000);
								sheetRecords.setColumnWidth((short)14, (short)1800);
								sheetRecords.setColumnWidth((short)15, (short)1200);
								sheetRecords.setColumnWidth((short)16, (short)3000);
								sheetRecords.setColumnWidth((short)17, (short)3000);
								sheetRecords.setColumnWidth((short)18, (short)3000);
								sheetRecords.setColumnWidth((short)19, (short)6000);
							}
							cell = row.createCell(nCell++);
							cell = row.createCell(nCell++);
							cell = row.createCell(nCell++);	cell.setCellValue(userView.getFieldLabel(WORKANDEXPENSERECORD_CLASS, "billableAmount", app.getCurrentLocaleAsIndex())); cell.setCellStyle(rightAlignStyle);
							cell = row.createCell(nCell++);	cell.setCellValue(userView.getFieldLabel(WORKANDEXPENSERECORD_CLASS, "isBillable", app.getCurrentLocaleAsIndex()));
							cell = row.createCell(nCell++);	cell.setCellValue(userView.getFieldLabel(WORKANDEXPENSERECORD_CLASS, "isReimbursable", app.getCurrentLocaleAsIndex()));
							if (isWorkRecord) {
								cell = row.createCell(nCell++);
								cell = row.createCell(nCell++);	cell.setCellValue(userView.getFieldLabel(WORKANDEXPENSERECORD_CLASS, "recordType", app.getCurrentLocaleAsIndex()));
							}

/*--------------------------------------------------------------
| R E C	O R D S
\--------------------------------------------------------------*/
%>

							<table class="table table-hover table-striped">
								<thead>
									<tr>
										<th class="" colspan="2">
											<%= userView.getFieldLabel(WORKANDEXPENSERECORD_CLASS, "startedAt", app.getCurrentLocaleAsIndex()) %>
										</th>
										<th class="<%= showFullStartedAtDate ? "" : "hidden" %>" colspan="2"><%= userView.getFieldLabel(WORKANDEXPENSERECORD_CLASS, "endedAt", app.getCurrentLocaleAsIndex()) %></th>
										<th class=""></td>
										<th class=""><%= userView.getFieldLabel(WORKANDEXPENSERECORD_CLASS, "activity", app.getCurrentLocaleAsIndex()) %>&nbsp;</th>
										<th class=""><%= userView.getFieldLabel(WORKANDEXPENSERECORD_CLASS, "name", app.getCurrentLocaleAsIndex()) %>&nbsp;</th>
										<th class="" <%= hasMultipleResources ? "" : "style='display:none;'" %>><%= userView.getFieldLabel(WORKANDEXPENSERECORD_CLASS, "resource", app.getCurrentLocaleAsIndex()) %>&nbsp;</th>
										<th class=""><%= isWorkRecord ? "hh:mm" : userView.getFieldLabel(WORKANDEXPENSERECORD_CLASS, "quantity", app.getCurrentLocaleAsIndex()) %></th>
<%
										if (!isWorkRecord) {
%>
											<th class=""><%= userView.getFieldLabel(WORKANDEXPENSERECORD_CLASS, "recordType", app.getCurrentLocaleAsIndex()) %>&nbsp;</th>
											<th class=""><%= userView.getFieldLabel(WORKANDEXPENSERECORD_CLASS, "quantityUom", app.getCurrentLocaleAsIndex()) %>&nbsp;</th>
<%
										}
										if (isWorkRecord) {
%>
											<th class="">&nbsp;</th>
											<th class="" colspan="2"><%= userView.getFieldLabel(WORKANDEXPENSERECORD_CLASS, "billableAmount", app.getCurrentLocaleAsIndex()) %></th>
											<th class="" title="<%= userView.getFieldLabel(WORKANDEXPENSERECORD_CLASS, "isBillable", app.getCurrentLocaleAsIndex()) %>">$&nbsp;</th>
											<th class="" title="<%= userView.getFieldLabel(WORKANDEXPENSERECORD_CLASS, "isReimbursable", app.getCurrentLocaleAsIndex()) %>">*&nbsp;</th>
											<th class=""><%= userView.getFieldLabel(WORKANDEXPENSERECORD_CLASS, "recordType", app.getCurrentLocaleAsIndex()) %>&nbsp;</th>
<%
										}
%>
										<th class=""><%= userView.getFieldLabel(WORKANDEXPENSERECORD_CLASS, "description", app.getCurrentLocaleAsIndex()) %>&nbsp;</th>
										<th class=""><%= userView.getFieldLabel(ACTIVITY_CLASS, "reportingAccount", app.getCurrentLocaleAsIndex()) %>&nbsp;</th>
										<th class=""></th>
										<th class=""></th>
										<th class=""></th>
									</tr>
								</thead>
								<tbody>
<%
								boolean isEvenRow = false;
								boolean isFirstRow = true;
								BigDecimal dailySum = BigDecimal.ZERO;
								Set resourcesToday = new TreeSet();
								String calDayName = null;
								String resourceErrorMsg = null;
								String startedAtCurrent = "";
								String startedAtPrevious = "";
								int rowCounter = 0;
								int wr = 0;
								for(Iterator i = workReportEntries.values().iterator(); i.hasNext(); wr++) {
									org.opencrx.kernel.activity1.jmi1.WorkAndExpenseRecord workAndExpenseRecord = (org.opencrx.kernel.activity1.jmi1.WorkAndExpenseRecord)i.next();

									GregorianCalendar startedAtDate = new GregorianCalendar(app.getCurrentLocale());
									startedAtDate.setTimeZone(TimeZone.getTimeZone(app.getCurrentTimeZone()));
									startedAtDate.setMinimalDaysInFirstWeek(4); // this conforms to DIN 1355/ISO 8601
									startedAtDate.setTime(workAndExpenseRecord.getStartedAt());
									startedAtCurrent = getDateAsString(startedAtDate);

									String recordHref = "";
									Action action = new Action(
											SelectObjectAction.EVENT_ID,
											new Action.Parameter[]{
													new Action.Parameter(Action.PARAMETER_OBJECTXRI, workAndExpenseRecord.refMofId())
											},
											"",
											true // enabled
										);
									recordHref = "../../" + action.getEncodedHRef();

									org.opencrx.kernel.activity1.jmi1.Activity activity = null;
									try {
										activity = workAndExpenseRecord.getActivity();
									} catch (Exception e) {}
									String activityHref = "";
									if (
										activity != null &&
										(activity.getPriority() >= priority)
									) {
										action = new Action(
											SelectObjectAction.EVENT_ID,
											new Action.Parameter[]{
													new Action.Parameter(Action.PARAMETER_OBJECTXRI, activity.refMofId())
											},
											"",
											true // enabled
										);
										activityHref = "../../" + action.getEncodedHRef();
										String resourceHref = "";
										if (workAndExpenseRecord.getResource() != null) {
											action = new Action(
													SelectObjectAction.EVENT_ID,
													new Action.Parameter[]{
															new Action.Parameter(Action.PARAMETER_OBJECTXRI, workAndExpenseRecord.getResource().refMofId())
													},
													"",
													true // enabled
											);
											resourceHref = "../../" + action.getEncodedHRef();
										}
										boolean isDayBreak = false;
										if (isFirstRow) {
												isDayBreak = false;
												isFirstRow = false;
										}


										// WorkRecordInPercent special treatment
										
										org.opencrx.kernel.activity1.jmi1.Resource currentResource = null;
										org.opencrx.kernel.activity1.jmi1.Calendar cal = null;
										org.opencrx.kernel.activity1.jmi1.CalendarDay calDay = null;
										calDayName = null;
										try {
											calDayName = calendardayf.format(startedAtDate.getTime());
										} catch (Exception e) {}
										String calDayLoad = null;
										double dayLoad = 100.0;
										short defaultLoad = 100;
										if (currentResource != null && calDayName != null) {
											String dayKey = calDayName + currentResource.refMofId();
											if (dayLoads.get(dayKey) == null) {
												dayLoads.put(dayKey, (Double)dayLoad);
											}
											if (workAndExpenseRecord.getQuantity() != null && workAndExpenseRecord.getQuantity().doubleValue() > 0) {
													resourcesToday.add(currentResource.refMofId());

													String activityTotalKey = activity.refMofId() + currentResource.refMofId();
													double contribution = workAndExpenseRecord.getQuantity().doubleValue();
													if (dayPercentageTotals.get(dayKey) == null) {
														dayPercentageTotals.put(dayKey, (Double)contribution);
													} else {
														dayPercentageTotals.put(dayKey, ((Double)dayPercentageTotals.get(dayKey)) + contribution);
													}

													contribution = contribution * dayLoad / 10000.0;
													if (activityTotals.get(activityTotalKey) == null) {
														activityTotals.put(activityTotalKey, (Double)contribution);
													} else {
														activityTotals.put(activityTotalKey, ((Double)activityTotals.get(activityTotalKey)) + contribution);
													}
													String resourceTotalKey = currentResource.refMofId();
													if (resourceTotals.get(resourceTotalKey) == null) {
														resourceTotals.put(resourceTotalKey, (Double)contribution);
													} else {
														resourceTotals.put(resourceTotalKey, ((Double)resourceTotals.get(resourceTotalKey)) + contribution);
													}
											}
										}
										double recordTotal = 0.0;
										boolean quantityError = false;
										try {
											if (workAndExpenseRecord.getQuantity() != null && workAndExpenseRecord.getRate() != null) {
													dailySum = dailySum.add(workAndExpenseRecord.getQuantity());
													recordTotal = workAndExpenseRecord.getQuantity().doubleValue() * workAndExpenseRecord.getRate().doubleValue();
											} else {
												quantityError = true;
												totalsError = true;
											}
										} catch (Exception e) {
											quantityError = true;
											totalsError = true;
										}
										if (workAndExpenseRecord.getBillingCurrency() == 0) {
											quantityError = true;
											totalsError = true;
										}
											String currency = DEFAULT_CURRENCY;
											try {
												 currency = (String)(codes.getShortText(FEATURE_BILLING_CURRENCY, app.getCurrentLocaleAsIndex(), true, true).get(new Short(workAndExpenseRecord.getBillingCurrency())));
											} catch (Exception e) {};
										row = sheetRecords.createRow(nRow++);
										nCell = 0;
										if (workAndExpenseRecord.getStartedAt() != null) {
											cell = row.createCell(nCell++);	cell.setCellValue(weekdayf.format(workAndExpenseRecord.getStartedAt())); cell.setCellStyle(rightAlignStyle);
											cell = row.createCell(nCell++);
											cell.setCellValue(workAndExpenseRecord.getStartedAt()); cell.setCellStyle(dateTimeStyle);
										} else {
											cell = row.createCell(nCell++);
											cell = row.createCell(nCell++);
										}
										if (workAndExpenseRecord.getEndedAt() != null) {
											cell = row.createCell(nCell++);	cell.setCellValue(weekdayf.format(workAndExpenseRecord.getEndedAt())); cell.setCellStyle(rightAlignStyle);
											cell = row.createCell(nCell++); cell.setCellValue(workAndExpenseRecord.getEndedAt()); cell.setCellStyle(dateTimeStyle);
										} else {
											cell = row.createCell(nCell++);
											cell = row.createCell(nCell++);
										}
										cell = row.createCell(nCell++);	cell.setCellValue(recordsWithoutAssignedActivityGroup.contains(workAndExpenseRecord) ? "!!!" : "");
										cell = row.createCell(nCell++);
										if (activity != null) {
											cell.setCellValue("#" + activity.getActivityNumber() + " " + activity.getName());
										}
										cell = row.createCell(nCell++);	cell.setCellValue(workAndExpenseRecord.getName());
										cell = row.createCell(nCell++);	cell.setCellValue(workAndExpenseRecord.getDescription() != null ? workAndExpenseRecord.getDescription() : "");
										cell = row.createCell(nCell++);	cell.setCellValue(workAndExpenseRecord.getResource() != null ? /*(new ObjectReference(workAndExpenseRecord.getResource(), app)).getTitle()*/ (workAndExpenseRecord.getResource().getName() != null ? workAndExpenseRecord.getResource().getName() : "?") : "");
										String reportingAccount = "";
										try {
												if (activity.getReportingAccount() != null) {
														reportingAccount = activity.getReportingAccount().getFullName();
												} else {
														if (activity.getReportingContact() != null) {
																reportingAccount = activity.getReportingContact().getFullName();
														}
												}
										} catch (Exception e) {
												new ServiceException(e).log();
										}
										cell = row.createCell(nCell++);	cell.setCellValue(reportingAccount);
										cell = row.createCell(nCell++);
										if (workAndExpenseRecord.getQuantity() != null) {
											if (isWorkRecord) {
												cell.setCellValue(workAndExpenseRecord.getQuantity().doubleValue() / 24.0); cell.setCellStyle(timeStyle);
											} else {
												cell.setCellValue(workAndExpenseRecord.getQuantity().doubleValue()); cell.setCellStyle(weightStyle);
											}
										}
										if (!isWorkRecord) {
												cell = row.createCell(nCell++);	cell.setCellValue(workAndExpenseRecord.getRecordType());
												cell = row.createCell(nCell++);	cell.setCellValue((codes.getLongTextByCode(FEATURE_RECORD_TYPE, app.getCurrentLocaleAsIndex(), true).get(new Short(workAndExpenseRecord.getRecordType()))));
												cell = row.createCell(nCell++);	cell.setCellValue(workAndExpenseRecord.getQuantityUom() != null && workAndExpenseRecord.getQuantityUom().getName() != null ? workAndExpenseRecord.getQuantityUom().getName() : "?");
										}
										cell = row.createCell(nCell++);
										if (workAndExpenseRecord.getRate() != null) {
											cell.setCellValue(workAndExpenseRecord.getRate().doubleValue()); cell.setCellStyle(amountStyle);
										}
										cell = row.createCell(nCell++);	cell.setCellValue(currency);
										cell = row.createCell(nCell++);	cell.setCellValue(recordTotal); cell.setCellStyle(amountStyle);
										cell = row.createCell(nCell++);	cell.setCellValue(workAndExpenseRecord.isBillable() != null && workAndExpenseRecord.isBillable().booleanValue());
										cell = row.createCell(nCell++);	cell.setCellValue(workAndExpenseRecord.isReimbursable() != null && workAndExpenseRecord.isReimbursable().booleanValue());
										if (isWorkRecord) {
											cell = row.createCell(nCell++);	cell.setCellValue(workAndExpenseRecord.getRecordType());
											cell = row.createCell(nCell++);	cell.setCellValue((codes.getLongTextByCode(FEATURE_RECORD_TYPE, app.getCurrentLocaleAsIndex(), true).get(new Short(workAndExpenseRecord.getRecordType()))));
										}
%>
										<tr>
											<td><a class="text-decoration-none" href='<%= recordHref %>' target='_blank'><%= workAndExpenseRecord.getStartedAt() != null ? weekdayf.format(workAndExpenseRecord.getStartedAt()) : "--" %>&nbsp;</a></td>
											<td class="padded_r" nowrap><a class="text-decoration-none" href='<%= recordHref %>' target='_blank'><%= workAndExpenseRecord.getStartedAt() != null ? (showFullStartedAtDate ? datetimef.format(workAndExpenseRecord.getStartedAt()) : dateonlyf.format(workAndExpenseRecord.getStartedAt())) : "--" %></a></td>
											<td <%= showFullStartedAtDate ? "" : "class='hidden'" %>"><a class="text-decoration-none" href='<%= recordHref %>' target='_blank'><%= workAndExpenseRecord.getEndedAt() != null ? weekdayf.format(workAndExpenseRecord.getEndedAt()) : "--" %>&nbsp;</a></td>
											<td class="padded_r <%= showFullStartedAtDate ? "" : "hidden" %>"><a class="text-decoration-none" href='<%= recordHref %>' target='_blank'><%= workAndExpenseRecord.getEndedAt() != null ? datetimef.format(workAndExpenseRecord.getEndedAt()) : "--" %></a></td>
											<td><%= recordsWithoutAssignedActivityGroup.contains(workAndExpenseRecord) ? CAUTION : "" %></td>
											<td style="white-space: nowrap;"><a class="text-decoration-none" href='<%= activityHref %>' target='_blank'>#<%= activity != null ? app.getHtmlEncoder().encode(new ObjectReference(activity, app).getTitle(), false) : "--" %>&nbsp;</a></td>
											<td style="white-space: nowrap;"><a class="text-decoration-none" href='<%= recordHref %>' target='_blank'><%= workAndExpenseRecord.getName() != null ? app.getHtmlEncoder().encode(workAndExpenseRecord.getName(), false) : "" %></a></td>
											<td <%= hasMultipleResources ? "" : "style='display:none;'" %>><a class="text-decoration-none" href='<%= resourceHref %>' target='_blank'><%= workAndExpenseRecord.getResource() != null ? app.getHtmlEncoder().encode(new ObjectReference(workAndExpenseRecord.getResource(), app).getTitle(), false) : "" %>&nbsp;</a></td>
											<td class="padded_r"><a class="text-decoration-none" href='<%= recordHref %>' target='_blank'><%= workAndExpenseRecord.getQuantity() == null ? "--" : (isWorkRecord ? decimalMinutesToHhMm(workAndExpenseRecord.getQuantity().doubleValue() * 60.0) : quantityf.format(workAndExpenseRecord.getQuantity())) %></a></td>
<%
											if (!isWorkRecord) {
%>
												<td><a class="text-decoration-none" href='<%= recordHref %>' target='_blank'><%= (codes.getLongTextByCode(FEATURE_RECORD_TYPE, app.getCurrentLocaleAsIndex(), true).get(new Short(workAndExpenseRecord.getRecordType()))) %></a></td>
												<td><a class="text-decoration-none" href='<%= recordHref %>' target='_blank'><%= workAndExpenseRecord.getQuantityUom() != null && workAndExpenseRecord.getQuantityUom().getName() != null ? app.getHtmlEncoder().encode(workAndExpenseRecord.getQuantityUom().getName(), false) : "?" %>&nbsp;</a></td>
<%
											}
											if (isWorkRecord) {
%>
												<td class="padded_r"><a class="text-decoration-none" href='<%= recordHref %>' target='_blank'>[<%= workAndExpenseRecord.getRate() != null ? ratesepf.format(workAndExpenseRecord.getRate()) : "--" %>]&nbsp;</a></td>
												<td class="padded_r" <%= quantityError ? ERROR_STYLE : "" %>><a class="text-decoration-none" href='<%= recordHref %>' target='_blank'><%= currency %></a></td>
												<td class="padded_r" <%= quantityError ? ERROR_STYLE : "" %>><a class="text-decoration-none" href='<%= recordHref %>' target='_blank'><%= ratesepf.format(recordTotal) %></a></td>
												<td><a class="text-decoration-none" href='<%= recordHref %>' target='_blank'><img src="../../images/<%= workAndExpenseRecord.isBillable() != null && workAndExpenseRecord.isBillable().booleanValue() ? "" : "not" %>checked_r.gif" /></a></td>
												<td><a class="text-decoration-none" href='<%= recordHref %>' target='_blank'><img src="../../images/<%= workAndExpenseRecord.isReimbursable() != null && workAndExpenseRecord.isReimbursable().booleanValue() ? "" : "not" %>checked_r.gif" /></a></td>
												<td style="white-space: nowrap;"><a class="text-decoration-none" href='<%= recordHref %>' target='_blank'><%= (codes.getLongTextByCode(FEATURE_RECORD_TYPE, app.getCurrentLocaleAsIndex(), true).get(new Short(workAndExpenseRecord.getRecordType()))) %></a></td>
<%
											}
%>
											<td><%= workAndExpenseRecord.getDescription() != null ? app.getHtmlEncoder().encode(workAndExpenseRecord.getDescription().replace("\n", "<br />"), false) : "" %></td>
											<td nowrap><a class="text-decoration-none" href='<%= activityHref %>' target='_blank'><%= reportingAccount %></a></td>
											<td class="padded_r"></td>
											<td class="padded_r"></td>
										</tr>
<%
										isEvenRow = !isEvenRow;
									}
								}
								if (resourceErrorMsg != null) {
										row = sheetRecords.createRow(nRow++);
										nCell = 0;
										cell = row.createCell(nCell++);	
										cell = row.createCell(nCell++);	
										cell.setCellValue(resourceErrorMsg);
										dayErrorMessages.put(calDayName, resourceErrorMsg);
%>
										<tr class='break'>
											<td colspan="12"><%= resourceErrorMsg %></td>
										</tr>
<%
								}
%>
								</tbody>
							</table>
<%
							{
%>
								<br />
								<h3>Total by Week</h3>
								<table class="table table-hover table-striped" style="width:70%">
<!-- totals per week -->
<%
								GregorianCalendar calendarBeginOfWeek = null;
								if (earliestDate != null && latestDate != null) {
									calendarBeginOfWeek = new GregorianCalendar(app.getCurrentLocale());
									calendarBeginOfWeek.setTimeZone(TimeZone.getTimeZone(app.getCurrentTimeZone()));
									calendarBeginOfWeek.setMinimalDaysInFirstWeek(4); // this conforms to DIN 1355/ISO 8601
									calendarBeginOfWeek.setTime(earliestDate);
									while (calendarBeginOfWeek.get(GregorianCalendar.DAY_OF_WEEK) != calendarBeginOfWeek.getFirstDayOfWeek()) {
											calendarBeginOfWeek.add(GregorianCalendar.DAY_OF_MONTH, -1);
									}
																calendarBeginOfWeek.set(GregorianCalendar.HOUR_OF_DAY, 0);
																calendarBeginOfWeek.set(GregorianCalendar.MINUTE, 0);
																calendarBeginOfWeek.set(GregorianCalendar.SECOND, 0);
																calendarBeginOfWeek.set(GregorianCalendar.MILLISECOND, 0);
								}

								nRow = REPORT_STARTING_ROW;
								row = sheetWeeks.createRow(nRow++);
								nCell = 0;
								cell = row.createCell(nCell);	cell.setCellValue(app.getLabel(CALENDAR_CLASS));
								sheetWeeks.setColumnWidth(nCell++, (short)12000);

%>
								<thead>
									<tr class="">
										<th>Week</th>
<%
										int dayCounter = 0;
										if (isWorkRecord && calendarBeginOfWeek != null) {
												GregorianCalendar wd = (GregorianCalendar)calendarBeginOfWeek.clone();
												for (int i = wd.get(GregorianCalendar.DAY_OF_WEEK); dayCounter < 7; dayCounter++) {
%>
													<th class="padded_r"><%= weekdayf.format(wd.getTime()) %></th>
<%
													cell = row.createCell(nCell);	cell.setCellValue(weekdayf.format(wd.getTime())); cell.setCellStyle(rightAlignStyle);
													sheetWeeks.setColumnWidth(nCell++, (short)2000);
													wd.add(GregorianCalendar.DAY_OF_MONTH, 1);
												}
										} else {
%>
											<th class="padded_r" colspan="7"></th>
<%
										}
										for (Iterator i = totals.keySet().iterator(); i.hasNext();) {
											String key = (String)i.next();
											if (!isWorkRecord && key.compareTo(timeKey) == 0) {continue;}
%>
											<th class="padded_r"><%= key %></th>
<%
											cell = row.createCell(nCell);	cell.setCellValue(key); cell.setCellStyle(rightAlignStyle);
											sheetWeeks.setColumnWidth(nCell++, (short)3000);
										}
%>
									</tr>
								</thead>
								<tbody>
<%
								double[] sumDays = new double[7 + totals.keySet().size()];
								for(int i = 0; i < sumDays.length; i++) {
										sumDays[i] = 0.0;
								}
								if (calendarBeginOfWeek != null) {
									GregorianCalendar currentDate = (GregorianCalendar)calendarBeginOfWeek.clone();
									while (currentDate.getTime().compareTo(latestDate) <= 0) {
										dayCounter = 0;
										GregorianCalendar beginOfCurrentWeek = (GregorianCalendar)currentDate.clone();
										GregorianCalendar endOfCurrentWeek = (GregorianCalendar)currentDate.clone();
										endOfCurrentWeek.add(GregorianCalendar.DAY_OF_MONTH, 6);
%>
										<tr>
											<td style="white-space: nowrap;"><%= yyyyf.format(beginOfCurrentWeek.getTime()) %> #<%= formatter2.format(beginOfCurrentWeek.get(GregorianCalendar.WEEK_OF_YEAR)) %> [<%= datef.format(beginOfCurrentWeek.getTime()) %> / <%= datef.format(endOfCurrentWeek.getTime()) %>]</td>
<%
											row = sheetWeeks.createRow(nRow++);
											nCell = 0;
											cell = row.createCell(nCell++);	cell.setCellValue(yyyyf.format(beginOfCurrentWeek.getTime()) + " #" + formatter2.format(beginOfCurrentWeek.get(GregorianCalendar.WEEK_OF_YEAR)) + " [" + datef.format(beginOfCurrentWeek.getTime()) + " / " + datef.format(endOfCurrentWeek.getTime()) + "]");
											if (isWorkRecord) {
												for(int i = currentDate.getFirstDayOfWeek(); dayCounter < 7; dayCounter++) {
														String WWDDKey = null;		// YYYYWWDD where WW week of year and DD day of week
														String WWDDsumKey = null; // YYYYWW99 (sum of week)
														String WWDDTimeKey = null;
														String WWDDsumTimeKey = null;
														try {
																WWDDTimeKey =	yyyyf.format(currentDate.getTime()) +
																							formatter2.format(currentDate.get(GregorianCalendar.WEEK_OF_YEAR)) +
																							formatter2.format(currentDate.get(GregorianCalendar.DAY_OF_WEEK) % 7) + "_" + timeKey;
														} catch (Exception e) {
																WWDDTimeKey = "?_" + timeKey;
														}
														boolean outOfPeriod = (reportBeginOfPeriod != null && currentDate.compareTo(reportBeginOfPeriod) < 0) ||
																									(reportEndOfPeriod != null && currentDate.compareTo(reportEndOfPeriod) > 0);
														cell = row.createCell(nCell++);
														if ((totalsPerWeekDay.get(WWDDTimeKey) != null) && !outOfPeriod) {
																cell.setCellValue(((Double)totalsPerWeekDay.get(WWDDTimeKey)) / 24.0); cell.setCellStyle(timeStyle);
														}
%>
														<td class="padded_r <%= outOfPeriod ? "outofperiod" : "" %>"><%= totalsPerWeekDay.get(WWDDTimeKey) == null ? (outOfPeriod ? "" : "--") : decimalMinutesToHhMm(((Double)totalsPerWeekDay.get(WWDDTimeKey)) * 60.0) %></td>
<%
														sumDays[currentDate.get(GregorianCalendar.DAY_OF_WEEK) % 7] += (totalsPerWeekDay.get(WWDDTimeKey) == null ? 0.0 : (Double)totalsPerWeekDay.get(WWDDTimeKey));
														currentDate.add(GregorianCalendar.DAY_OF_MONTH, 1);
												}
											} else {
%>
												<td class="padded_r" colspan="7">&nbsp;</td>
<%
												currentDate.add(GregorianCalendar.DAY_OF_MONTH, 7);
											}
											int sumIdx = 6;
											for (Iterator i = totals.keySet().iterator(); i.hasNext();) {
													String key = (String)i.next();
													sumIdx++;
													if (!isWorkRecord && key.compareTo(timeKey) == 0) {continue;}
													String sumKey = null; // YYYYWW99 (sum of week)
													try {
															sumKey = yyyyf.format(beginOfCurrentWeek.getTime()) +
																			formatter2.format(beginOfCurrentWeek.get(GregorianCalendar.WEEK_OF_YEAR)) +
																			"99_" + key;
													} catch (Exception e) {
															sumKey = "9_" + key;
													}
													cell = row.createCell(nCell++);
													if (totalsPerWeekDay.get(sumKey) != null) {
															if (key.compareTo(timeKey) == 0) {
																	cell.setCellValue(((Double)totalsPerWeekDay.get(sumKey)) / 24.0); cell.setCellStyle(timeStyle);
															} else {
																	cell.setCellValue((Double)totalsPerWeekDay.get(sumKey)); cell.setCellStyle(amountStyle);
															}

													}
%>
													<td class="padded_r"><%= totalsPerWeekDay.get(sumKey) == null ? "--" : (key.compareTo(timeKey) == 0 ? decimalMinutesToHhMm(60.0 * (Double)totalsPerWeekDay.get(sumKey)) : ratesepf.format((Double)totalsPerWeekDay.get(sumKey))) %></td>
<%
													sumDays[sumIdx] += (totalsPerWeekDay.get(sumKey) == null ? 0.0 : (Double)totalsPerWeekDay.get(sumKey));
											}
%>
										</tr>
<%
									}
								}
%>
								<tr>
<%
									row = sheetWeeks.createRow(nRow++);
									nCell = 0;
									cell = row.createCell(nCell++);	cell.setCellValue("Total");
									if (isWorkRecord && calendarBeginOfWeek != null) {
%>
										<td class="total">Total</td>
<%
											dayCounter = 0;
											for(int i = calendarBeginOfWeek.getFirstDayOfWeek(); dayCounter < 7; dayCounter++) {
												cell = row.createCell(nCell++);
												cell.setCellValue(sumDays[i % 7] / 24.0); cell.setCellStyle(timeStyle);
%>
												<td class="totalR"><%= decimalMinutesToHhMm(sumDays[i % 7] * 60.0) %></td>
<%
												i++;
											}
									} else {
%>
											<td class="total" colspan="8">Total</td>
<%
									}
									int sumIdx = 6;
									for (Iterator i = totals.keySet().iterator(); i.hasNext();) {
											String key = (String)i.next();
											sumIdx++;
											if (!isWorkRecord && key.compareTo(timeKey) == 0) {continue;}
											cell = row.createCell(nCell++);
											if (key.compareTo(timeKey) == 0) {
													cell.setCellValue(sumDays[sumIdx] / 24.0); cell.setCellStyle(timeStyle);
											} else {
													cell.setCellValue(sumDays[sumIdx]); cell.setCellStyle(amountStyle);
											}
%>
											<td class="totalR"><%= key.compareTo(timeKey) == 0 ? decimalMinutesToHhMm(60.0 * sumDays[sumIdx]) : ratesepf.format(sumDays[sumIdx]) %></td>
<%
									}
%>
								</tr>
<!-- end totals per week -->

<!-- simple totals -->
								<tr>
									<td colspan="8">Total (<%= userView.getFieldLabel(WORKANDEXPENSERECORD_CLASS, "isBillable", app.getCurrentLocaleAsIndex()) %>)</td>
<%
									row = sheetWeeks.createRow(nRow++);
									nCell = 0;
									cell = row.createCell(nCell++);	cell.setCellValue(userView.getFieldLabel(WORKANDEXPENSERECORD_CLASS, "isBillable", app.getCurrentLocaleAsIndex()));
									if (isWorkRecord) {nCell = 8;}
									for (Iterator i = totalsBillable.keySet().iterator(); i.hasNext();) {
										String key = (String)i.next();
										if (!isWorkRecord && key.compareTo(timeKey) == 0) {continue;}
										cell = row.createCell(nCell++);
										if (key.compareTo(timeKey) == 0) {
												cell.setCellValue(((Double)totalsBillable.get(key)) / 24.0); cell.setCellStyle(timeStyle);
										} else {
												cell.setCellValue((Double)totalsBillable.get(key)); cell.setCellStyle(amountStyle);
										}
%>
										<td class="padded_r"><%= key.compareTo(timeKey) == 0 ? decimalMinutesToHhMm(60.0 * (Double)totalsBillable.get(key)) : ratesepf.format((Double)totalsBillable.get(key)) %></td>
<%
									}
%>
								</tr>

								<tr>
									<td colspan="8">Total (<%= userView.getFieldLabel(WORKANDEXPENSERECORD_CLASS, "isReimbursable", app.getCurrentLocaleAsIndex()) %>)</td>
<%
									row = sheetWeeks.createRow(nRow++);
									nCell = 0;
									cell = row.createCell(nCell++);	cell.setCellValue(userView.getFieldLabel(WORKANDEXPENSERECORD_CLASS, "isReimbursable", app.getCurrentLocaleAsIndex()));
									if (isWorkRecord) {nCell = 8;}
									for (Iterator i = totalsReimbursable.keySet().iterator(); i.hasNext();) {
										String key = (String)i.next();
										if (!isWorkRecord && key.compareTo(timeKey) == 0) {continue;}
										cell = row.createCell(nCell++);
										if (key.compareTo(timeKey) == 0) {
												cell.setCellValue(((Double)totalsReimbursable.get(key)) / 24.0); cell.setCellStyle(timeStyle);
										} else {
												cell.setCellValue((Double)totalsReimbursable.get(key)); cell.setCellStyle(amountStyle);
										}
%>
										<td class="padded_r"><%= key.compareTo(timeKey) == 0 ? decimalMinutesToHhMm(60.0 * (Double)totalsReimbursable.get(key)) : ratesepf.format((Double)totalsReimbursable.get(key)) %></td>
<%
									}
%>
								</tr>
<!-- end simple totals -->
<%
								}

/*--------------------------------------------------------------
| A C T I V I T I E S
\--------------------------------------------------------------*/
%>
<!-- totals per activity -->
							</tbody>
							</table>

							<br />
							<h3>Total by Activity</h3>
							<table class="table table-hover table-striped" style="width:70%">
								<thead>
								<tr class="">
									<th class="" colspan="8">Activity</th>
<%
									nRow = REPORT_STARTING_ROW;
									row = sheetActivities.createRow(nRow++);
									nCell = 0;
									cell = row.createCell(nCell);	cell.setCellValue(app.getLabel(ACTIVITYSEGMENT_CLASS));
									sheetActivities.setColumnWidth(nCell++, (short)12000);
									for (Iterator i = totals.keySet().iterator(); i.hasNext();) {
										String key = (String)i.next();
										if (!isWorkRecord && key.compareTo(timeKey) == 0) {continue;}
%>
										<th class="padded_r"><%= key %></th>
<%
										cell = row.createCell(nCell);	cell.setCellValue(key); cell.setCellStyle(rightAlignStyle);
										sheetActivities.setColumnWidth(nCell++, (short)3000);
									}
%>
								</tr>
								</thead>
								<tbody>
<%
								for (Iterator a = activities.keySet().iterator(); a.hasNext();) {
									String activityNumber = (String)a.next();
									org.opencrx.kernel.activity1.jmi1.Activity activity = (org.opencrx.kernel.activity1.jmi1.Activity)pm.getObjectById(new Path((String)activities.get(activityNumber)));
									String activityHref = "";
									Action action = new Action(
											SelectObjectAction.EVENT_ID,
											new Action.Parameter[]{
													new Action.Parameter(Action.PARAMETER_OBJECTXRI, activity.refMofId())
											},
											"",
											true // enabled
										);
									activityHref = "../../" + action.getEncodedHRef();
									row = sheetActivities.createRow(nRow++);
									nCell = 0;
									cell = row.createCell(nCell++);	cell.setCellValue("#" + activity.getActivityNumber() + " " + activity.getName());
%>
									<tr>
										<td colspan="8"><a class="text-decoration-none" href='<%= activityHref %>' target='_blank'>#<%= app.getHtmlEncoder().encode(new ObjectReference(activity, app).getTitle(), false) %></a></td>
<%
										for (Iterator i = totals.keySet().iterator(); i.hasNext();) {
											String key = activityNumber + "_" + (String)i.next();
											String activityKey = activityNumber + "_" + timeKey;
											if (!isWorkRecord && key.compareTo(activityKey) == 0) {continue;}
											cell = row.createCell(nCell++);
											if (totalsPerActivity.get(key) != null) {
													if (key.compareTo(activityKey) == 0) {
															cell.setCellValue(((Double)totalsPerActivity.get(key)) / 24.0); cell.setCellStyle(timeStyle);
													} else {
															cell.setCellValue((Double)totalsPerActivity.get(key)); cell.setCellStyle(amountStyle);
													}
											}
%>
											<td class="padded_r"><%= totalsPerActivity.get(key) == null ? "--" : (key.compareTo(activityKey) == 0 ? decimalMinutesToHhMm(60.0 * (Double)totalsPerActivity.get(key)) : ratesepf.format((Double)totalsPerActivity.get(key))) %></td>
<%
										}
%>
									</tr>
<%
								}
								{
%>
									<tr>
										<td class="total" colspan="8">Total</td>
<%
										row = sheetActivities.createRow(nRow++);
										nCell = 0;
										cell = row.createCell(nCell++);	cell.setCellValue("Total");
										for (Iterator i = totals.keySet().iterator(); i.hasNext();) {
											String key = (String)i.next();
											if (!isWorkRecord && key.compareTo(timeKey) == 0) {continue;}
											cell = row.createCell(nCell++);
											if (key.compareTo(timeKey) == 0) {
													cell.setCellValue(((Double)totals.get(key)) / 24.0); cell.setCellStyle(timeStyle);
											} else {
													cell.setCellValue((Double)totals.get(key)); cell.setCellStyle(amountStyle);
											}
%>
											<td class="totalR"><%= key.compareTo(timeKey) == 0 ? decimalMinutesToHhMm(60.0 * (Double)totals.get(key)) : ratesepf.format((Double)totals.get(key)) %></td>
<%
										}
%>
									</tr>
<%
								}
%>
<!--	end totals per activity -->
							</tbody>
							</table>							
<%
							{
%>
<!-- totals per activityGroup -->
								<br />
								<h3>Total by Activity Group</h3>
								<table class="table table-hover table-striped" style="width:70%">
								<thead>
									<tr class="">
										<th class="" colspan="8">Activity Group</th>
<%
										nRow = REPORT_STARTING_ROW;
										row = sheetActivityGroups.createRow(nRow++);
										nCell = 0;
										cell = row.createCell(nCell);	cell.setCellValue(app.getLabel(ACTIVITYTRACKER_CLASS) + " / " + app.getLabel(ACTIVITYCATEGORY_CLASS) + " / " + app.getLabel(ACTIVITYMILESTONE_CLASS));
										sheetActivityGroups.setColumnWidth(nCell++, (short)12000);
										for (Iterator i = totals.keySet().iterator(); i.hasNext();) {
											String key = (String)i.next();
											if (!isWorkRecord && key.compareTo(timeKey) == 0) {continue;}
											cell = row.createCell(nCell);	cell.setCellValue(key); cell.setCellStyle(rightAlignStyle);
											sheetActivityGroups.setColumnWidth(nCell++, (short)3000);
%>
											<th class="padded_r"><%= key %></th>
<%
										}
%>
									</tr>
								</thead>
								<tbody>
<%
								for (Iterator a = activityGroups.keySet().iterator(); a.hasNext();) {
										String agkey = (String)a.next();
										org.opencrx.kernel.activity1.jmi1.ActivityGroup actGroup = (org.opencrx.kernel.activity1.jmi1.ActivityGroup)pm.getObjectById(new Path((String)activityGroups.get(agkey)));
										String actGroupHref = "";
										Action action = new Action(
												SelectObjectAction.EVENT_ID,
												new Action.Parameter[]{
														new Action.Parameter(Action.PARAMETER_OBJECTXRI, actGroup.refMofId())
												},
												"",
												true // enabled
											);
										actGroupHref = "../../" + action.getEncodedHRef();
										row = sheetActivityGroups.createRow(nRow++);
										nCell = 0;
										cell = row.createCell(nCell++);	cell.setCellValue(actGroup.getName());
%>
										<tr>
											<td colspan="8"><a class="text-decoration-none" href='<%= actGroupHref %>' target='_blank'><%= app.getHtmlEncoder().encode(new ObjectReference(actGroup, app).getTitle(), false) %></a></td>
<%
											for (Iterator i = totals.keySet().iterator(); i.hasNext();) {
												String key = (actGroup.getName() != null ? actGroup.getName().toUpperCase() : "") + GAP_BEFORE_XRI + actGroup.refMofId() + "_" + (String)i.next();
												String activityGroupKey = (actGroup.getName() != null ? actGroup.getName().toUpperCase() : "") + GAP_BEFORE_XRI + actGroup.refMofId() + "_" + timeKey;
												if (!isWorkRecord && key.compareTo(activityGroupKey) == 0) {continue;}
												cell = row.createCell(nCell++);
												if (totalsPerActivityGroup.get(key) != null) {
														if (key.compareTo(activityGroupKey) == 0) {
																cell.setCellValue(((Double)totalsPerActivityGroup.get(key)) / 24.0); cell.setCellStyle(timeStyle);
														} else {
																cell.setCellValue((Double)totalsPerActivityGroup.get(key)); cell.setCellStyle(amountStyle);
														}
												}
%>
												<td class="padded_r"><%= totalsPerActivityGroup.get(key) == null ? "--" : (key.compareTo(activityGroupKey) == 0 ? decimalMinutesToHhMm(60.0 * (Double)totalsPerActivityGroup.get(key)) : ratesepf.format((Double)totalsPerActivityGroup.get(key))) %></td>
<%
											}
%>
										</tr>
<%
								}
%>
<!--	end totals per activityGroup -->
<%
								if (isProjectReporting) {
										sheetProject = addSheet(wb, "Project", false, labels, values, 1);

%>
<!-- totals per projectPhase -->
										<tr class="">
											<td class="" colspan="8"><%= userView.getFieldLabel(ACTIVITYTRACKER_CLASS, "userBoolean0", app.getCurrentLocaleAsIndex()) %>: <%= projectTracker.getName() != null ? app.getHtmlEncoder().encode(projectTracker.getName(), false) : "?" %></td>
<%
											nRow = REPORT_STARTING_ROW;
											row = sheetProject.createRow(nRow++);
											nCell = 0;
											cell = row.createCell(nCell);	cell.setCellValue(userView.getFieldLabel(ACTIVITYTRACKER_CLASS, "userBoolean0", app.getCurrentLocaleAsIndex()) + ": " + (projectTracker.getName() != null ? projectTracker.getName() : "?"));
											sheetProject.setColumnWidth(nCell++, (short)6000);
											sheetProject.setColumnWidth(nCell++, (short)12000);
											sheetProject.setColumnWidth(nCell++, (short)6000);
											for (Iterator i = totals.keySet().iterator(); i.hasNext();) {
												String key = (String)i.next();
												if (!isWorkRecord && key.compareTo(timeKey) == 0) {continue;}
												cell = row.createCell(nCell);	cell.setCellValue(key); cell.setCellStyle(rightAlignStyle);
												sheetProject.setColumnWidth(nCell++, (short)3000);
%>
												<td class="padded_r"><%= key %></td>
<%
											}
											sheetProject.setColumnWidth(nCell++, (short)2000);
											sheetProject.setColumnWidth(nCell++, (short)4000);
%>
										</tr>
<%
										for (Iterator a = projectPhaseList.values().iterator(); a.hasNext();) {
												org.opencrx.kernel.activity1.jmi1.ActivityGroup actGroup = (org.opencrx.kernel.activity1.jmi1.ActivityGroup)a.next();
												String actGroupHref = "";
												Action action = new Action(
														SelectObjectAction.EVENT_ID,
														new Action.Parameter[]{
																new Action.Parameter(Action.PARAMETER_OBJECTXRI, actGroup.refMofId())
														},
														"",
														true // enabled
													);
												actGroupHref = "../../" + action.getEncodedHRef();
												row = sheetProject.createRow(nRow++);
												nCell = 0;
												cell = row.createCell(nCell++);	cell.setCellValue(actGroup.getName());
												cell = row.createCell(nCell++);
												cell = row.createCell(nCell++);
%>
												<tr>
													<td colspan="8"><a class="text-decoration-none" href='<%= actGroupHref %>' target='_blank'><%= app.getHtmlEncoder().encode(new ObjectReference(actGroup, app).getTitle(), false) %></a></td>
<%
													for (Iterator i = totals.keySet().iterator(); i.hasNext();) {
														String projectPhaseKey = (actGroup.getName() != null ? actGroup.getName().toUpperCase() : "") + GAP_BEFORE_XRI + actGroup.refMofId() + "_" + (String)i.next();
														String projectPhaseTimeKey = (actGroup.getName() != null ? actGroup.getName().toUpperCase() : "") + GAP_BEFORE_XRI + actGroup.refMofId() + "_" + timeKey;
														if (!isWorkRecord && projectPhaseKey.compareTo(projectPhaseTimeKey) == 0) {continue;}
														cell = row.createCell(nCell++);
														if (totalsPerProjectPhase.get(projectPhaseKey) != null) {
																if (projectPhaseKey.compareTo(projectPhaseTimeKey) == 0) {
																		cell.setCellValue(((Double)totalsPerProjectPhase.get(projectPhaseKey)) / 24.0); cell.setCellStyle(timeStyle);
																} else {
																		cell.setCellValue((Double)totalsPerProjectPhase.get(projectPhaseKey)); cell.setCellStyle(amountStyle);
																}
														}
%>
														<td class="padded_r"><%= totalsPerProjectPhase.get(projectPhaseKey) == null ? "--" : (projectPhaseKey.compareTo(projectPhaseTimeKey) == 0 ? decimalMinutesToHhMm(60.0 * (Double)totalsPerProjectPhase.get(projectPhaseKey)) : ratesepf.format((Double)totalsPerProjectPhase.get(projectPhaseKey))) %></td>
<%
													}
%>
												</tr>
<%
										}
%>
										<tr>
											<td class="total" colspan="8">Total</td>
<%
											row = sheetProject.createRow(nRow++);
											nCell = 0;
											cell = row.createCell(nCell++);	cell.setCellValue("Total");
											cell = row.createCell(nCell++);
											cell = row.createCell(nCell++);
											for (Iterator i = totalsProject.keySet().iterator(); i.hasNext();) {
												String key = (String)i.next();
												if (!isWorkRecord && key.compareTo(timeKey) == 0) {continue;}
												cell = row.createCell(nCell++);
												if (key.compareTo(timeKey) == 0) {
														cell.setCellValue(((Double)totalsProject.get(key)) / 24.0); cell.setCellStyle(timeStyle);
												} else {
														cell.setCellValue((Double)totalsProject.get(key)); cell.setCellStyle(amountStyle);
												}
%>
												<td class="totalR"><%= key.compareTo(timeKey) == 0 ? decimalMinutesToHhMm(60.0 * (Double)totalsProject.get(key)) : ratesepf.format((Double)totalsProject.get(key)) %></td>
<%
											}
%>
										</tr>
<!--	end totals per projectPhase -->
<%
								}
							}
%>
							</tbody>
							</table>
<%
							if (isProjectReporting) {
%>
<!-- totals per projectReportingLine -->
									<br />
									<p>
									<table class="table table-hover table-striped">
										<thead>
											<tr class="">
												<th class=""><%= userView.getFieldLabel(ACTIVITYTRACKER_CLASS, "userBoolean0", app.getCurrentLocaleAsIndex()) %>: <%= projectTracker.getName() != null ? app.getHtmlEncoder().encode(projectTracker.getName(), false) : "?" %></th>
												<th class=""><%= userView.getFieldLabel(WORKANDEXPENSERECORD_CLASS, "activity", app.getCurrentLocaleAsIndex()) %>&nbsp;</th>
												<th class=""><%= userView.getFieldLabel(WORKANDEXPENSERECORD_CLASS, "resource", app.getCurrentLocaleAsIndex()) %>&nbsp;</th>
<%
											row = sheetProject.createRow(nRow++);
											row = sheetProject.createRow(nRow++);
											row = sheetProject.createRow(nRow++);
											row = sheetProject.createRow(nRow++);
											row = sheetProject.createRow(nRow++);
											row = sheetProject.createRow(nRow++);
											nCell = 0;
											cell = row.createCell(nCell++);	cell.setCellValue(userView.getFieldLabel(ACTIVITYTRACKER_CLASS, "userBoolean0", app.getCurrentLocaleAsIndex()) + ": " + (projectTracker.getName() != null ? projectTracker.getName() : "?"));
											cell = row.createCell(nCell++);	cell.setCellValue(userView.getFieldLabel(WORKANDEXPENSERECORD_CLASS, "activity", app.getCurrentLocaleAsIndex()));
											cell = row.createCell(nCell++);	cell.setCellValue(userView.getFieldLabel(WORKANDEXPENSERECORD_CLASS, "resource", app.getCurrentLocaleAsIndex()));
											for (Iterator i = totalsReportingLines.keySet().iterator(); i.hasNext();) {
												String key = (String)i.next();
												if (!isWorkRecord && key.compareTo(timeKey) == 0) {continue;}
												cell = row.createCell(nCell++);	cell.setCellValue(key); cell.setCellStyle(rightAlignStyle);
%>
												<th class=""><%= key %></th>
<%
											}
											if (!isWorkRecord) {
												cell = row.createCell(nCell++);
												cell = row.createCell(nCell++); cell.setCellValue(userView.getFieldLabel(WORKANDEXPENSERECORD_CLASS, "recordType", app.getCurrentLocaleAsIndex()));
%>
												<th class=""><%= userView.getFieldLabel(WORKANDEXPENSERECORD_CLASS, "recordType", app.getCurrentLocaleAsIndex()) %>&nbsp;</th>
<%
											} else {
												cell = row.createCell(nCell++);	cell.setCellValue(userView.getFieldLabel(WORKANDEXPENSERECORD_CLASS, "rate", app.getCurrentLocaleAsIndex())); cell.setCellStyle(rightAlignStyle);
%>
												<th class=""><%= userView.getFieldLabel(WORKANDEXPENSERECORD_CLASS, "rate", app.getCurrentLocaleAsIndex()) %></th>
<%
											}
%>
											</tr>
										</thead>
										<tbody>
<%
										for (Iterator r = reportingLines.values().iterator(); r.hasNext();) {
												String key = (String)r.next();
												String rlActivityGroup = "?";
												String titleActivityGroup = "";
												String rlActivity = "?";
												String titleActivity = "";
												String rlResource = "?";
												String titleResource = "";
												short rlRecordType = 0;
												String rlRate = "?";
												boolean hasError = false;
												String reportingLineKEY = null;
												try {
														String keyParts[] = key.split(KEY_SPLITTER);

														// recover ActivityGroup
														org.opencrx.kernel.activity1.jmi1.ActivityGroup lActivityGroup = (org.opencrx.kernel.activity1.jmi1.ActivityGroup)pm.getObjectById(new Path(keyParts[0]));
														if (lActivityGroup != null) {
															Action action = new Action(
																	SelectObjectAction.EVENT_ID,
																	new Action.Parameter[]{
																			new Action.Parameter(Action.PARAMETER_OBJECTXRI, lActivityGroup.refMofId())
																	},
																	"",
																	true // enabled
																);
															rlActivityGroup = "../../" + action.getEncodedHRef();
															rlActivityGroup = "<a class='text-decoration-none' href='" + rlActivityGroup + "' target='_blank'>" + app.getHtmlEncoder().encode(new ObjectReference(lActivityGroup, app).getTitle(), false) + "</a>";
															titleActivityGroup = lActivityGroup.getName();
														}

														// recover Activity
														org.opencrx.kernel.activity1.jmi1.Activity lActivity = (org.opencrx.kernel.activity1.jmi1.Activity)pm.getObjectById(new Path(keyParts[1]));
														if (lActivity != null) {
															Action action = new Action(
																	SelectObjectAction.EVENT_ID,
																	new Action.Parameter[]{
																			new Action.Parameter(Action.PARAMETER_OBJECTXRI, lActivity.refMofId())
																	},
																	"",
																	true // enabled
																);
															rlActivity = "../../" + action.getEncodedHRef();
															rlActivity = "<a class='text-decoration-none' href='" + rlActivity + "' target='_blank'>" + app.getHtmlEncoder().encode(new ObjectReference(lActivity, app).getTitle(), false) + "</a>";
															titleActivity = "#" + lActivity.getActivityNumber() + " " + lActivity.getName();
														}

														// recover Resource
														org.opencrx.kernel.activity1.jmi1.Resource lResource = (org.opencrx.kernel.activity1.jmi1.Resource)pm.getObjectById(new Path(keyParts[2]));
														if (lResource != null) {
															Action action = new Action(
																	SelectObjectAction.EVENT_ID,
																	new Action.Parameter[]{
																			new Action.Parameter(Action.PARAMETER_OBJECTXRI, lResource.refMofId())
																	},
																	"",
																	true // enabled
																);
															rlResource = "../../" + action.getEncodedHRef();
															rlResource = "<a class='text-decoration-none' href='" + rlResource + "' target='_blank'>" + app.getHtmlEncoder().encode(new ObjectReference(lResource, app).getTitle(), false) + "</a>";
															titleResource = lResource.getName();
														}

														// recover recordType
														rlRecordType = Short.parseShort(keyParts[3]);

														// recover rate
														rlRate = ratesepf.format(new java.math.BigDecimal(keyParts[4]));
												} catch (Exception e) {
														hasError = true;
												}
												row = sheetProject.createRow(nRow++);
												nCell = 0;
												cell = row.createCell(nCell++);	cell.setCellValue(titleActivityGroup);
												cell = row.createCell(nCell++); cell.setCellValue(titleActivity);
												cell = row.createCell(nCell++); cell.setCellValue(titleResource);
%>
												<tr <%= hasError ? ERROR_STYLE : "" %>>
													<td><%= rlActivityGroup %></td>
													<td><%= rlActivity %></td>
													<td><%= rlResource %></td>
<%
													for (Iterator i = totalsReportingLines.keySet().iterator(); i.hasNext();) {
														String reportLineKey =	key + KEY_SPLITTER +	"_" + (String)i.next();
														String reportLineTimeKey =	key + KEY_SPLITTER +	"_" + timeKey;
														if (!isWorkRecord && reportLineKey.compareTo(reportLineTimeKey) == 0) {continue;}
														cell = row.createCell(nCell++);
														if (totalsPerProjectReportLine.get(reportLineKey) != null) {
																if (reportLineKey.compareTo(reportLineTimeKey) == 0) {
																		cell.setCellValue(((Double)totalsPerProjectReportLine.get(reportLineKey)) / 24.0); cell.setCellStyle(timeStyle);
																} else {
																		cell.setCellValue((Double)totalsPerProjectReportLine.get(reportLineKey)); cell.setCellStyle(amountStyle);
																}
														}
%>
														<td class="padded_r"><%= totalsPerProjectReportLine.get(reportLineKey) == null ? "--" : (reportLineKey.compareTo(reportLineTimeKey) == 0 ? decimalMinutesToHhMm(60.0 * (Double)totalsPerProjectReportLine.get(reportLineKey)) : ratesepf.format((Double)totalsPerProjectReportLine.get(reportLineKey))) %></td>
<%
													}
													if (!isWorkRecord) {
														cell = row.createCell(nCell++);	cell.setCellValue(rlRecordType);
														cell = row.createCell(nCell++);	cell.setCellValue((codes.getLongTextByCode(FEATURE_RECORD_TYPE, app.getCurrentLocaleAsIndex(), true).get(new Short(rlRecordType))));
%>
														<td><%= (codes.getLongTextByCode(FEATURE_RECORD_TYPE, app.getCurrentLocaleAsIndex(), true).get(new Short(rlRecordType))) %></td>
<%
													} else {
														cell = row.createCell(nCell++);	cell.setCellValue(rlRate); cell.setCellStyle(amountStyle);
%>
														<td class="padded_r"><%= rlRate %></td>
<%
													}
%>
												</tr>
<%
										}
%>
										<tr>
											<td class="total" colspan="3">Total</td>
<%
											row = sheetProject.createRow(nRow++);
											nCell = 0;
											cell = row.createCell(nCell++);	cell.setCellValue("Total");
											cell = row.createCell(nCell++);
											cell = row.createCell(nCell++);
											for (Iterator i = totalsReportingLines.keySet().iterator(); i.hasNext();) {
												String key = (String)i.next();
												if (!isWorkRecord && key.compareTo(timeKey) == 0) {continue;}
												cell = row.createCell(nCell++);
												if (key.compareTo(timeKey) == 0) {
														cell.setCellValue(((Double)totalsReportingLines.get(key)) / 24.0); cell.setCellStyle(timeStyle);
												} else {
														cell.setCellValue((Double)totalsReportingLines.get(key)); cell.setCellStyle(amountStyle);
												}
%>
												<td class="totalR"><%= key.compareTo(timeKey) == 0 ? decimalMinutesToHhMm(60.0 * (Double)totalsReportingLines.get(key)) : ratesepf.format((Double)totalsReportingLines.get(key)) %></td>
<%
											}
%>
											<td class="total"></td>
										</tr>
									</tbody>
									</table>
<!--	end totals per projectReportingLine -->
<%
							}
							wb.write(os);
							os.flush();
							os.close();
					} /* doReportCalculation */
				}
%>
				</form>
				<br>
				<script language="javascript" type="text/javascript">
						$('WaitIndicator').style.display='none';
						$('SubmitArea').style.display='block';
						function setFocus(id) {
							try {
								$(id).focus();
							} catch(e){}
						}
<%
						if (isContactChange)		{ %>setFocus('contactXri.Title');	<% }
						if (isResourceChange)		{ %>setFocus('resourceXri');			<% }
						if (isResourceChange)		{ %>setFocus('resourceXri');			<% }

						if (isFirstCall)			{ %>setFocus('contactXri.Title');	<% }
%>
				</script>



							</div> <!-- inspPanel0 -->
						</div> <!-- inspContent -->
					</div> <!-- inspector -->
				</div> <!-- aPanel -->

			</div> <!-- content -->
		</div> <!-- content-wrap -->
	</div> <!-- wrap -->
</div> <!-- container -->
</body>
</html>
<%
if(pm != null) {
	pm.close();
}
%>
