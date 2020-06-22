<%@page contentType="text/html;charset=UTF-8" language="java" pageEncoding="UTF-8" %>
<%@taglib prefix="t" tagdir="/WEB-INF/tags" %>
<%
/**
 * ====================================================================
 * Project:     openCRX/Core, http://www.opencrx.org/
 * Description:	Create Work Record
 * Owner:		CRIXP Corp., Switzerland, http://www.crixp.com
 * ====================================================================
 *
 * This software is published under the BSD license
 * as listed below.
 *
 * Copyright (c) 2009-2016, CRIXP Corp., Switzerland
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
%><%@ page session="true" import="
java.util.*,
java.io.*,
java.text.*,
org.opencrx.portal.wizard.*,
org.opencrx.kernel.generic.*,
org.openmdx.kernel.id.cci.*,
org.openmdx.kernel.id.*,
org.openmdx.base.exception.*,
org.openmdx.base.accessor.jmi.cci.*,
org.openmdx.portal.servlet.*,
org.openmdx.portal.servlet.attribute.*,
org.openmdx.portal.servlet.component.*,
org.openmdx.portal.servlet.control.*,
org.openmdx.portal.servlet.wizards.*,
org.openmdx.base.naming.*
" %>
<%
	final boolean SHOW_ERRORS = false;
	final int MAX_ACTIVITY_SHOWN_INITIALLY = 50;
	final int MAX_ACTIVITY_SHOWN = 500;
	final int MAX_ACTIVITY_SORT_ORDER = 4;
	final String FORM_NAME = "CreateWorkAndExpenseRecord";
	final String WIZARD_NAME = FORM_NAME + ".jsp";
	final String ONFOCUS_HANDLER = "javascript:$('lastFocusId').value=this.id;";
	final String CAUTION = "<img border='0' alt='' height='16px' src='../../images/caution.gif' />";

	final boolean EXCLUDE_ACTIVITYTRACKER_TEMPLATES = true; // excludes ActivityTracker if ActivityTracker.userBoolean1 == true

	final String featureRecordType = "workAndExpenseType";
	final String featureRecordTypeWork = "workAndExpenseTypeWorkOnly";
	final String featureRecordTypeExpense = "workAndExpenseTypeExpenseOnly";
	final String featureBillingCurrency = "currency";
	final String featurePaymentType = "paymenttype";

	final String contactTargetFinder = "org:opencrx:kernel:activity1:Resource:contact";

	final String errorStyle = "style='background-color:#FFF0CC;'";
	final String errorStyleInline = "background-color:#FFF0CC;";
	
	CreateWorkAndExpenseRecordController wc = new CreateWorkAndExpenseRecordController();
%>
	<t:wizardHandleCommand controller='<%= wc %>' defaultCommand='Refresh' />
<%
	if(response.getStatus() != HttpServletResponse.SC_OK) {
		wc.close();		
		return;
	}
	ApplicationContext app = wc.getApp();
	javax.jdo.PersistenceManager pm = wc.getPm();
	RefObject_1_0 obj = wc.getObject();
	
	int tabIndex = 1000; // calendar
	boolean mustReload = false;
	GregorianCalendar firstDayOfMonth = wc.getFirstDayOfMonthCal(wc.getFormFields().getSelectedDateStr());
	boolean isWorkRecord = Boolean.TRUE.equals(wc.getFormFields().getIsWorkRecord());
	boolean isWorkRecordInPercent = Boolean.TRUE.equals(wc.getFormFields().getIsWorkRecordInPercent());
	boolean isInitialized = Boolean.TRUE.equals(wc.getFormFields().getIsInitialized());
	String name = wc.getFormFields().getName();
	String selectedDateStr = wc.getFormFields().getSelectedDateStr();
	String activityXri = wc.getFormFields().getActivityXri();
	String activityFilterXri = wc.getFormFields().getActivityFilterXri();
	Boolean makePrivate = wc.getFormFields().getMakePrivate();
	Short billingCurrency = wc.getFormFields().getBillingCurrency();
%>
<!DOCTYPE HTML PUBLIC "-//W3C//DTD HTML 4.01 Transitional//EN" "http://www.w3.org/TR/html4/loose.dtd">
<html dir="<%= wc.getTexts().getDir() %>">
<head>
	<title>openCRX - Create Work/Expense Record</title>
	<meta http-equiv="Content-Type" content="text/html; charset=utf-8">
	<link rel="stylesheet" href="../../js/bootstrap/css/bootstrap.min.css">	
	<link rel="stylesheet" href="../../_style/colors.css">
	<link rel="stylesheet" href="../../_style/n2default.css">
	<link rel="stylesheet" href="../../_style/ssf.css">
	<link rel='shortcut icon' href='../../images/favicon.ico' />
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

		function percentageTick(currentVal, upVal) {
			try {
				pVal = parseInt(currentVal, 10);
			} catch (e) {}
			if (isNaN(pVal)) {pVal=100;}
			if (upVal > 0) {
				if (pVal + upVal > 100) {
					pVal = 100;
				} else {
					pVal = pVal + upVal;
				}
			}
			if (upVal < 0) {
				if (pVal + upVal < 0) {
					pVal = 0;
				} else {
					pVal = pVal + upVal;
				}
			}
			return pVal;
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
		fieldset{
			margin: 0px 10px 20px 0px;
			padding: 5px 0px 5px 15px;
			-moz-border-radius: 10px;
			-webkit-border-radius: 10px;
			border: 1.5px solid #DDD;
			background-color: #EEE;
		}
		.small{font-size:8pt;}
		#wizMonth {
			text-align:center;
			white-space:nowrap;
		}
		input.error{background-color:red;}
		#scheduleTable, .fieldGroup {
			border-collapse: collapse;
			border-spacing:0;
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
		TD.smallheader{border-bottom:1px solid black;padding:0px 8px 0px 0px;font-weight:bold;}
		TD.smallheaderR{border-bottom:1px solid black;padding:0px 16px 0px 0px;font-weight:bold;text-align:right;}
		TD.miniheader{font-size:7pt;}
		TD.padded{padding:0px 15px 0px 0px;}
		TD.padded_l{padding:0px 15px 0px 15px;text-align:left;}
		TD.padded_r{padding:0px 15px 0px 0px;text-align:right;}
		TR.centered TD {text-align:center;}
		TR.leftaligned TD {text-align:left;}
		TR.even TD {background-color:#EEEEFF;}
		TR.match TD {background-color:#FFFE70;}
		TR.created TD {background-color:#B4FF96;font-weight:bold;}
		TD.error {color:red;font-weight:bold;}
		input.outofmonth {
			background-color:#F3F3F3;
		}
		input.outofmonth:hover {
			background-color:#80FF00;
		}
		input.selectedday {
			background-color:#FFFE70;
		}
		input.selectedday:hover {
			background-color:#80FF00;
		}
		input.selectedweek {
			background-color:#9797FF;
		}
		input.selectedweek:hover {
			background-color:#80FF00;
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
		input.time {
			width: 45px;
			text-align:right;
			font-weight:bold;
		}
		input.percentage {
			width: 40px;
			text-align:right;
			font-weight:bold;
		}
		.timeButtonL {
			cursor:pointer;
			padding:0px 0px 2px 10px;
			vertical-align:bottom;
		}
		.timeButtonR {
			cursor:pointer;
			padding:0px 10px 2px 0px;
			vertical-align:bottom;
		}
		input.quantity {
			width: 50px;
			text-align:right;
			font-weight:bold;
		}
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

						<ul class="nav nav-tabs nav-condensed" style="position:relative;z-index:1001;">
							<li class="<%= isWorkRecord && !isWorkRecordInPercent ? "active" : "hidden-print" %>"><a href="#" onclick="javascript:$('isWorkRecord').checked=true;$('isWorkRecordInPercent').checked=false;$('Reload.button').click();" href="#">Work Report</a></li>
							<li class="<%= isWorkRecord && isWorkRecordInPercent  ? "active" : "hidden-print" %>"><a href="#" onclick="javascript:$('isWorkRecord').checked=true;$('isWorkRecordInPercent').checked=true;$('Reload.button').click();" href="#">Work Report in %</a></li>
							<li class="<%= isWorkRecord                           ? "hidden-print" : "active" %>"><a href="#" onclick="javascript:$('isWorkRecord').checked=false;$('isWorkRecordInPercent').checked=false;$('Reload.button').click();" href="#">Expense Report</a></li>
						</ul>
						<div id="inspContent" class="inspContent" style="z-index: 200;">
							<div id="inspPanel0" class="selected" style="padding-top: 10px;">
								<form name="<%= FORM_NAME %>" accept-charset="UTF-8" method="POST" action="<%= "../.." + request.getServletPath() %>">
									<input type="hidden" name="Command" id="Command" value=""/>
									<input type="hidden" name="<%= Action.PARAMETER_REQUEST_ID %>" value="<%= wc.getRequestId() %>" />
									<input type="hidden" name="<%= Action.PARAMETER_OBJECTXRI %>" value="<%= wc.getObjectIdentity().toXRI() %>" />
									<input type="hidden" name="selectedDateStr" id="selectedDateStr" value="<%= wc.getFormFields().getSelectedDateStr() %>" />
									<input type="hidden" name="dayOfMonth" id="dayOfMonth" value="" />
									<input type="checkbox" name="isWorkRecord" id="isWorkRecord" style="display:none;" <%= isWorkRecord ? "checked" : "" %> />
									<input type="checkbox" name="isWorkRecordInPercent" id="isWorkRecordInPercent" style="display:none;" <%= isWorkRecordInPercent ? "checked" : "" %> />
									<input type="checkbox" name="hasProjects" id="hasProjects" style="display:none;" <%= Boolean.TRUE.equals(wc.getFormFields().getHasProjects()) ? "checked" : "" %> />
									<input type="checkbox" name="isContactChange" id="isContactChange" style="display:none;" />
									<input type="checkbox" name="isResourceChange" id="isResourceChange" style="display:none;" />
									<input type="checkbox" name="resetActivityXri" id="resetActivityXri" style="display:none;" />
									<input type="hidden" name="activitySortOrder" id="activitySortOrder" value="<%= Integer.toString(wc.getFormFields().getActivitySortOrder()) %>" />
									<input type="checkbox" style="display:none;" id="isInitialized" name="isInitialized" checked />
									<input type="hidden" name="deleteWorkRecordXri" id="deleteWorkRecordXri" value="" />
									<input type="hidden" name="lastCreatedWorkExpenseRecordXri" value="<%=wc.getFormFields().getLastCreatedWorkExpenseRecordXri()%>" />
									<input type="hidden" name="lastFocusId" id="lastFocusId" value="<%=wc.getFormFields().getLastFocusId()%>" />
									<input type="hidden" name="updateCalendarDay" id="updateCalendarDay" value="" />
									<input type="hidden" name="createCalendarDay" id="createCalendarDay" value="" />
									<input style="position:absolute;left:-500px;" type="submit" id="Reload.button" name="Reload" tabindex="<%=tabIndex++%>" value="<%=app.getTexts().getReloadText()%>" onclick="javascript:if($('Command').value==''){$('Command').value=this.name;};" />
									<table id="scheduleTable">
										<tr>
											<td style="width:240px;">
												<!--	Calendar -->
												<table>
													<tr><td>
														<div id="wizMonth" style="width:100%;">
															<table style="width:100%;">
																<tr>
																	<td>
																		<input id="Button.PrevYear" name="PrevYear" type="submit" style="font-size:6px;" tabindex="<%=tabIndex++%>" class="abutton" value="&lt;&lt;" onclick="javascript:$('Command').value=this.name;" />
																		<input id="Button.PrevMonth" name="PrevMonth" type="submit" style="font-size:6px;" tabindex="<%=tabIndex++%>" class="abutton" value="&nbsp;&nbsp;&lt;&nbsp;"	onclick="javascript:$('Command').value=this.name;" />
																	</td>
																	<td style="width:100%;vertical-align:middle;">
																		<span style="font-weight:bold;">&nbsp;<%=wc.getMonthFormat().format(firstDayOfMonth.getTime()) + " " + firstDayOfMonth.get(Calendar.YEAR)%>&nbsp;</span>
																	</td>
																	<td>
																		<input id="Button.NextMonth" name="NextMonth" type="submit" style="font-size:6px;" tabindex="<%=tabIndex++%>" class="abutton" value="&nbsp;&gt;&nbsp;&nbsp;" onclick="javascript:$('Command').value=this.name;" />
																		<input id="Button.NextYear" name="NextYear" type="submit" style="font-size:6px;" tabindex="<%=tabIndex++%>" class="abutton" value="&gt;&gt;" onclick="javascript:$('Command').value=this.name;" />
																	</td>
																</tr>
															</table>
														</div>
													</td></tr>
													<tr><td>
														<table id="calWizard" cellspacing="1">
															<thead>
																<tr>
																	<th style="text-align:center;padding:0px 10px;cursor:pointer;" title="today" onclick="javascript:$('selectedDateStr').value='<%=wc.getDateAsString(new GregorianCalendar())%>';$('Reload.button').click();">#</th>
<%
																	GregorianCalendar dayInWeekCalendar = (GregorianCalendar)firstDayOfMonth.clone();
																	while(dayInWeekCalendar.get(GregorianCalendar.DAY_OF_WEEK) != dayInWeekCalendar.getFirstDayOfWeek()) {
																		dayInWeekCalendar.add(GregorianCalendar.DAY_OF_MONTH, 1);
																	}
																	for(int i = 0; i < 7; i++) {
%>
																		<th style="text-align:right;"><%=wc.getDayInWeekFormat().format(dayInWeekCalendar.getTime())%>&nbsp;</th>
<%
																		dayInWeekCalendar.add(GregorianCalendar.DAY_OF_MONTH, 1);
																	}
%>
																</tr>
															</thead>
															<tbody>
<%
															int selectedWeekOfYear = -1;
															if (wc.getFormFields().getSelectedDateStr() != null) {
																selectedWeekOfYear = firstDayOfMonth.get(GregorianCalendar.WEEK_OF_YEAR);
															}
															GregorianCalendar calendarPrevMonth = new GregorianCalendar(app.getCurrentLocale());
															calendarPrevMonth.setTimeZone(TimeZone.getTimeZone(app.getCurrentTimeZone()));
															calendarPrevMonth.setMinimalDaysInFirstWeek(4); // this conforms to DIN 1355/ISO 8601
															calendarPrevMonth.set(GregorianCalendar.YEAR, firstDayOfMonth.get(Calendar.YEAR));
															calendarPrevMonth.set(GregorianCalendar.MONTH, firstDayOfMonth.get(Calendar.MONTH));
															calendarPrevMonth.set(GregorianCalendar.DAY_OF_MONTH, 1);
															while (calendarPrevMonth.get(GregorianCalendar.DAY_OF_WEEK) != calendarPrevMonth.getFirstDayOfWeek()) {
																calendarPrevMonth.add(GregorianCalendar.DAY_OF_MONTH, -1);
															}
															GregorianCalendar calendar = (GregorianCalendar)firstDayOfMonth.clone();
															int calendarMonth = firstDayOfMonth.get(Calendar.MONTH);
															while(calendar.get(GregorianCalendar.MONTH) == firstDayOfMonth.get(Calendar.MONTH)) {
%>
																<tr>
																	<td style="text-align:right;font-size:6pt;vertical-align:middle;padding:0px 10px;"><%=calendar.get(GregorianCalendar.WEEK_OF_YEAR)%></td>
<%
																		for(int i = 0; i < 7; i++) {
																			int dayOfMonth = calendar.get(GregorianCalendar.DAY_OF_MONTH);
																			if(((i + calendar.getFirstDayOfWeek() - 1) % 7 + 1) != calendar.get(GregorianCalendar.DAY_OF_WEEK)) {
																				int prevMonthDayOfMonth = calendarPrevMonth.get(GregorianCalendar.DAY_OF_MONTH);
																				String cssClass = selectedWeekOfYear == calendarPrevMonth.get(GregorianCalendar.WEEK_OF_YEAR)
																					? "selectedweek"
																					: "outofmonth";
%>
																				<td style="text-align:right;"><input id="SelectDateP.Button" tabindex="<%=tabIndex++%>" name="SelectDateP" type="submit" class="abutton <%=cssClass%>" style="padding:4px;" value="&nbsp;<%=prevMonthDayOfMonth < 10 ? "&nbsp; " : ""%><%=prevMonthDayOfMonth%>&nbsp;" onclick="javascript:$('Command').value=this.name;$('dayOfMonth').value='<%=prevMonthDayOfMonth%>';" /></td>
<%
																				calendarPrevMonth.add(GregorianCalendar.DAY_OF_MONTH, 1);
																			}
																			else {
																				String dateAsString = wc.getDateAsString(
																					calendar.get(Calendar.YEAR),
																					calendar.get(Calendar.MONTH)+1,
																					calendar.get(Calendar.DAY_OF_MONTH)
																				);
																				String cssClass = (wc.getFormFields().getSelectedDateStr() != null) && (wc.getFormFields().getSelectedDateStr().equals(dateAsString))
																					? (calendar.get(GregorianCalendar.MONTH) != calendarMonth ? "outofmonth" : "selectedday")
																					: (selectedWeekOfYear == calendar.get(GregorianCalendar.WEEK_OF_YEAR)
																							? "selectedweek"
																							: (calendar.get(GregorianCalendar.MONTH) != calendarMonth ? "outofmonth" : "bookable")
																						 );
																				String buttonName = calendar.get(GregorianCalendar.MONTH) != calendarMonth
																					? "SelectDateN"
																					: "SelectDate";
%>
																				<td style="text-align:right;"><input id="<%=buttonName%>.Button" tabindex="<%=tabIndex++%>" name="<%=buttonName%>" type="submit" class="abutton <%=cssClass%>" style="padding:4px;" value="&nbsp;<%=dayOfMonth < 10 ? "&nbsp; " : ""%><%=dayOfMonth%>&nbsp;" onclick="javascript:$('Command').value=this.name;$('dayOfMonth').value='<%=dayOfMonth%>';" /></td>
<%
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
											<td>&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;</td>
											<td style="width:100%;">
												<fieldset>
												<table class="fieldGroup">
													<tr>
														<td class="<%= CssClass.fieldLabel %>"><span class="nw"><%=app.getLabel(CreateWorkAndExpenseRecordController.CONTACT_CLASS)%>:</span></td>
<%
														tabIndex = 1;
														String lookupId = org.opencrx.kernel.backend.Accounts.getInstance().getUidAsString();
														Action findContactTargetObjectAction = Action.getFindObjectAction(contactTargetFinder, lookupId);
														String accountName = app.getLabel(CreateWorkAndExpenseRecordController.CONTACT_CLASS);
%>
														<td>
															<div class="autocompleterMenu">
																<ul id="<%=CssClass.ssf_nav %>" class="<%=CssClass.ssf_nav %>" onmouseover="sfinit(this);" >
																	<li><a href="#"><img border="0" alt="" src="../../images/autocomplete_select.png" /></a>
																		<ul onclick="this.style.left='-999em';" onmouseout="this.style.left='';">
																			<li class="selected"><a href="#" onclick="javascript:navSelect(this);ac_addObject0.url= './'+getEncodedHRef(['../../ObjectInspectorServlet', 'event', '40', 'parameter', 'xri*(xri:@openmdx:org.opencrx.kernel.account1/provider/<%=wc.getProviderName()%>/segment/<%=wc.getSegmentName()%>)*referenceName*(account)*filterByType*(org:opencrx:kernel:account1:Contact)*filterByFeature*(fullName)*filterOperator*(IS_LIKE)*orderByFeature*(fullName)*position*(0)*size*(20)']);return false;"><span>&nbsp;&nbsp;&nbsp;</span><%=accountName%> / <%=wc.getFieldLabel(CreateWorkAndExpenseRecordController.ACCOUNT_CLASS, "fullName", app.getCurrentLocaleAsIndex())%></a></li>
																			<li <%=wc.getFieldLabel(CreateWorkAndExpenseRecordController.ACCOUNT_CLASS, "description", app.getCurrentLocaleAsIndex()) == null ? "style='display:none;" : ""%>><a href="#" onclick="javascript:navSelect(this);ac_addObject0.url= './'+getEncodedHRef(['../../ObjectInspectorServlet', 'event', '40', 'parameter', 'xri*(xri:@openmdx:org.opencrx.kernel.account1/provider/<%=wc.getProviderName()%>/segment/<%=wc.getSegmentName()%>)*referenceName*(account)*filterByType*(org:opencrx:kernel:account1:Contact)*filterByFeature*(description)*filterOperator*(IS_LIKE)*orderByFeature*(description)*position*(0)*size*(20)']);return false;"><span>&nbsp;&nbsp;&nbsp;</span><%=accountName%> / <%=wc.getFieldLabel(CreateWorkAndExpenseRecordController.ACCOUNT_CLASS, "description", app.getCurrentLocaleAsIndex())%></a></li>
																			<li <%=wc.getFieldLabel(CreateWorkAndExpenseRecordController.ACCOUNT_CLASS, "aliasName", app.getCurrentLocaleAsIndex()) == null ? "style='display:none;" : ""%>><a href="#" onclick="javascript:navSelect(this);ac_addObject0.url= './'+getEncodedHRef(['../../ObjectInspectorServlet', 'event', '40', 'parameter', 'xri*(xri:@openmdx:org.opencrx.kernel.account1/provider/<%=wc.getProviderName()%>/segment/<%=wc.getSegmentName()%>)*referenceName*(account)*filterByType*(org:opencrx:kernel:account1:Contact)*filterByFeature*(aliasName)*filterOperator*(IS_LIKE)*orderByFeature*(aliasName)*position*(0)*size*(20)']);return false;"><span>&nbsp;&nbsp;&nbsp;</span><%=accountName%> / <%=wc.getFieldLabel(CreateWorkAndExpenseRecordController.ACCOUNT_CLASS, "aliasName", app.getCurrentLocaleAsIndex())%></a></li>
																			<li><a href="#" onclick="javascript:navSelect(this);ac_addObject0.url= './'+getEncodedHRef(['../../ObjectInspectorServlet', 'event', '40', 'parameter', 'xri*(xri:@openmdx:org.opencrx.kernel.account1/provider/<%=wc.getProviderName()%>/segment/<%=wc.getSegmentName()%>)*referenceName*(account)*filterByType*(org:opencrx:kernel:account1:Contact)*filterByFeature*(firstName)*filterOperator*(IS_LIKE)*orderByFeature*(firstName)*position*(0)*size*(20)']);return false;"><span>&nbsp;&nbsp;&nbsp;</span><%=accountName%> / <%=wc.getFieldLabel(CreateWorkAndExpenseRecordController.CONTACT_CLASS, "firstName", app.getCurrentLocaleAsIndex())%></a></li>
																			<li><a href="#" onclick="javascript:navSelect(this);ac_addObject0.url= './'+getEncodedHRef(['../../ObjectInspectorServlet', 'event', '40', 'parameter', 'xri*(xri:@openmdx:org.opencrx.kernel.account1/provider/<%=wc.getProviderName()%>/segment/<%=wc.getSegmentName()%>)*referenceName*(account)*filterByType*(org:opencrx:kernel:account1:Contact)*filterByFeature*(middleName)*filterOperator*(IS_LIKE)*orderByFeature*(middleName)*position*(0)*size*(20)']);return false;"><span>&nbsp;&nbsp;&nbsp;</span><%=accountName%> / <%=wc.getFieldLabel(CreateWorkAndExpenseRecordController.CONTACT_CLASS, "middleName", app.getCurrentLocaleAsIndex())%></a></li>
																			<li><a href="#" onclick="javascript:navSelect(this);ac_addObject0.url= './'+getEncodedHRef(['../../ObjectInspectorServlet', 'event', '40', 'parameter', 'xri*(xri:@openmdx:org.opencrx.kernel.account1/provider/<%=wc.getProviderName()%>/segment/<%=wc.getSegmentName()%>)*referenceName*(account)*filterByType*(org:opencrx:kernel:account1:Contact)*filterByFeature*(lastName)*filterOperator*(IS_LIKE)*orderByFeature*(lastName)*position*(0)*size*(20)']);return false;"><span>&nbsp;&nbsp;&nbsp;</span><%=accountName%> / <%=wc.getFieldLabel(CreateWorkAndExpenseRecordController.CONTACT_CLASS, "lastName", app.getCurrentLocaleAsIndex())%></a></li>
																			<li <%=wc.getFieldLabel(CreateWorkAndExpenseRecordController.CONTACT_CLASS, "nickName", app.getCurrentLocaleAsIndex()) == null ? "style='display:none;" : ""%>><a href="#" onclick="javascript:navSelect(this);ac_addObject0.url= './'+getEncodedHRef(['../../ObjectInspectorServlet', 'event', '40', 'parameter', 'xri*(xri:@openmdx:org.opencrx.kernel.account1/provider/<%=wc.getProviderName()%>/segment/<%=wc.getSegmentName()%>)*referenceName*(account)*filterByType*(org:opencrx:kernel:account1:Contact)*filterByFeature*(nickName)*filterOperator*(IS_LIKE)*orderByFeature*(nickName)*position*(0)*size*(20)']);return false;"><span>&nbsp;&nbsp;&nbsp;</span><%=accountName%> / <%=wc.getFieldLabel(CreateWorkAndExpenseRecordController.CONTACT_CLASS, "nickName", app.getCurrentLocaleAsIndex())%></a></li>
																		</ul>
																	</li>
																</ul>
															</div>
															<div class="autocompleterInput"><input type="text" class="valueL mandatory valueAC <%=wc.getContact() == null ? "inputError" : ""%>" id="contactXri.Title" name="contactXri.Title" tabindex="<%=tabIndex++%>" value="<%=wc.getFormFields().getContactXriTitle() != null ? wc.getFormFields().getContactXriTitle() : ""%>" onfocus="<%=ONFOCUS_HANDLER%>" /></div>
															<input type="hidden" class="valueLLocked" id="contactXri" readonly="true" name="contactXri" value="<%=wc.getFormFields().getContactXri() != null ? wc.getFormFields().getContactXri() : ""%>" />
															<div class="autocomplete" id="contact.Update" style="display:none;z-index:500;"></div>
															<script type="text/javascript" language="javascript" charset="utf-8">
																function afterUpdateReload(titleField, selectedItem) {
																	updateXriField(titleField, selectedItem);
																	$('isContactChange').checked="true";
																	$('resetActivityXri').checked="true";
																	$('Reload.button').click();
																}
																ac_addObject0 = new Ajax.Autocompleter(
																	'contactXri.Title',
																	'contact.Update',
																	'../../ObjectInspectorServlet?event=40&parameter=xri*%28xri%3A%40openmdx%3Aorg.opencrx.kernel.account1%2Fprovider%2F<%=wc.getProviderName()%>%2Fsegment%2F<%=wc.getSegmentName()%>%29*referenceName*%28account%29*filterByType*%28org%3Aopencrx%3Akernel%3Aaccount1%3AContact%29*filterByFeature*%28fullName%29*filterOperator*%28IS_LIKE%29*orderByFeature*%28fullName%29*position*%280%29*size*%2820%29',
																	{
																		paramName: 'filtervalues',
																		minChars: 0,
																		afterUpdateElement: afterUpdateReload
																	}
																);
															</script>
														</td>
														<td class="addon">
															<img class="popUpButton" border="0" alt="" src="../../images/closeInsp.gif" style="float:right;" onclick="javascript:$('contactXri').value='*';$('isContactChange').checked='true';$('contactXri.Title').value='*';$('resetActivityXri').checked=true;$('Reload.button').click();" />
															<img class="popUpButton" border="0" align="bottom" alt="Click to open ObjectFinder" src="../../images/lookup.gif" onclick="OF.findObject('../../<%=findContactTargetObjectAction.getEncodedHRef()%>', $('contactXri.Title'), $('contactXri'), '<%=lookupId%>');$('isContactChange').checked=true;" />
														</td>
													</tr>
													<tr>
														<td class="<%= CssClass.fieldLabel %>">
															<span class="nw"><%=app.getLabel(CreateWorkAndExpenseRecordController.RESOURCE_CLASS)%>:</span>
														</td>
														<td>
<%
															boolean noResourcesFound = false;
															String resourceXri = "";
															org.opencrx.kernel.activity1.cci2.ResourceQuery resourceFilter = 
																(org.opencrx.kernel.activity1.cci2.ResourceQuery)wc.getPm().newQuery(org.opencrx.kernel.activity1.jmi1.Resource.class);
															if (!Boolean.TRUE.equals(wc.getShowAllResources())) {
																resourceFilter.thereExistsContact().equalTo(wc.getContact());
															}
															resourceFilter.forAllDisabled().isFalse();
															resourceFilter.orderByName().ascending();
															resourceFilter.orderByDescription().ascending();
															List resources = wc.getActivitySegment().getResource(resourceFilter);
															if (resources.isEmpty()) {
																wc.appendErrorMsg("no matching resource found!<br>");
																noResourcesFound = true;
																resourceXri = "";
%>
																<select id="resourceXri" name="resourceXri" class="valueL" <%=errorStyle%> tabindex="<%=tabIndex++%>" onfocus="<%=ONFOCUS_HANDLER%>">
																	<option value="">--</option>
																</select>
<%
															} else {
																if (resourceXri == null) {mustReload = true;}
%>
																<select id="resourceXri" name="resourceXri" class="valueL" tabindex="<%=tabIndex++%>" onchange="javascript:$('isResourceChange').checked=true;$('resetActivityXri').checked=true;$('Reload.button').click();" onfocus="<%=ONFOCUS_HANDLER%>">
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
																		if (((resourceXri == null) || resourceXri.isEmpty()) && !Boolean.TRUE.equals(wc.getShowAllResourcesOfContact())) {
																			resourceXri = res.refGetPath().toXRI();
																		}
%>
																		<option <%=(resourceXri != null) && resourceXri.equals(res.refGetPath().toXRI()) ? "selected" : ""%> value="<%=res.refGetPath().toXRI()%>"><%=res.getName() != null ? app.getHtmlEncoder().encode(res.getName(), false) : contactTitle%><%=Boolean.TRUE.equals(wc.getShowAllResources()) ? " [" + contactTitle + "]" : ""%></option>
<%
																	}
%>
																</select>
<%
															}
%>
															<input type="hidden" name="previousResourceXri" value="<%=resourceXri%>" />
														</td>
														<td class="addon">
															<%=noResourcesFound ? CAUTION : ""%>
														</td>
													</tr>				
													<tr>
														<td class="<%= CssClass.fieldLabel %>">
															<span class="nw"><%=app.getLabel(CreateWorkAndExpenseRecordController.ACTIVITYFILTERGROUP_CLASS)%>:</span>
														</td>
														<td nowrap>
<%
															org.opencrx.kernel.activity1.jmi1.ActivityGroup activityGroup = wc.getActivityGroup();
															List activities = null;
															boolean openOnly = Boolean.TRUE.equals(wc.getFormFields().getExcludeClosedActivities());
															org.opencrx.kernel.activity1.cci2.ActivityQuery activityQuery = (org.opencrx.kernel.activity1.cci2.ActivityQuery)pm.newQuery(org.opencrx.kernel.activity1.jmi1.Activity.class);
															activityQuery.forAllDisabled().isFalse();
															if (openOnly) {
																activityQuery.activityState().lessThan(
																	new Short((short)20) // Status "not closed"
																);
															}
															switch (wc.getFormFields().getActivitySortOrder()) {
																case 0: activityQuery.orderByActivityNumber().ascending(); break;
																case 1: activityQuery.orderByActivityNumber().descending(); break;
																case 2: activityQuery.orderByName().ascending(); break;
																case 3: activityQuery.orderByName().descending(); break;
																default: activityQuery.orderByActivityNumber().descending(); break;
															}
															if (CreateWorkAndExpenseRecordController.ACTIVITY_FILTER_SEGMENT.equals(wc.getFormFields().getActivityFilter())) {
																activities = wc.getActivitySegment().getActivity(activityQuery);
																activityGroup = null; // ensure that all activities are shown
															} else {
																tabIndex += 10;
																if(!CreateWorkAndExpenseRecordController.ACTIVITY_FILTER_PROJECT.equals(wc.getFormFields().getActivityFilter())) {
																	if (wc.getSortedActivityGroups() == null || wc.getSortedActivityGroups().isEmpty()) {
																		activityGroup = null; // reset potentially existing selection
																		wc.appendErrorMsg("no activity groups found!<br>");
%>
																		<select class="valueL" style="width:50%;float:right;" id="activityFilterXri" name="activityFilterXri" <%=errorStyle%> tabindex="<%=tabIndex+5%>" onfocus="<%=ONFOCUS_HANDLER%>">
																			<option value="">--</option>
																		</select>
<%
																	} else {
%>
																		<select class="valueL" style="width:50%;float:right;" id="activityFilterXri" name="activityFilterXri" tabindex="<%=tabIndex+5%>" onchange="javascript:$('resetActivityXri').checked=true;$('Reload.button').click();" onfocus="<%=ONFOCUS_HANDLER%>">
<%
																			boolean hasSelection = false;
																			org.opencrx.kernel.activity1.jmi1.ActivityGroup firstAg = null;
																			for(org.opencrx.kernel.activity1.jmi1.ActivityGroup ag: wc.getSortedActivityGroups()) {
																				if (
																					!EXCLUDE_ACTIVITYTRACKER_TEMPLATES || !(ag instanceof org.opencrx.kernel.activity1.jmi1.ActivityTracker) ||
																					((((org.opencrx.kernel.activity1.jmi1.ActivityTracker)ag).isUserBoolean1() == null) || (!((org.opencrx.kernel.activity1.jmi1.ActivityTracker)ag).isUserBoolean1().booleanValue()))
																				) {
																					boolean selected = false;
																					if(activityFilterXri != null && activityFilterXri.equals(ag.refGetPath().toXRI())) {
																						activityGroup = ag;
																						selected = true;
																						hasSelection = true;
																					}
																					if (firstAg == null) {
																						firstAg = ag;
																					}
%>
																					<option <%=activityFilterXri != null && activityFilterXri.equals(ag.refGetPath().toXRI()) ? "selected" : ""%> value="<%=ag.refGetPath().toXRI()%>"><%=app.getHtmlEncoder().encode(ag.getName(), false)%></option>
<%
																				}
																			}
																			if (!hasSelection) {
																				activityGroup = firstAg; // to ensure proper location of activities
																				activityFilterXri = firstAg.refGetPath().toXRI();
																			}
%>
																		</select>
<%
																	}
																}
															}
%>
															<select class="valueL" style="width:<%=CreateWorkAndExpenseRecordController.ACTIVITY_FILTER_SEGMENT.equals(wc.getFormFields().getActivityFilter()) || CreateWorkAndExpenseRecordController.ACTIVITY_FILTER_PROJECT.equals(wc.getFormFields().getActivityFilter()) ? "100" : "49"%>%;float:left;" id="activityFilter" name="activityFilter" tabindex="<%=tabIndex++%>" onchange="javascript:$('resetActivityXri').checked=true;$('Reload.button').click();" onfocus="<%=ONFOCUS_HANDLER%>">
																<option <%=CreateWorkAndExpenseRecordController.ACTIVITY_FILTER_SEGMENT.equals(wc.getFormFields().getActivityFilter()) ? "selected" : ""%> value="<%=CreateWorkAndExpenseRecordController.ACTIVITY_FILTER_SEGMENT%>"	>*</option>
																<option <%=CreateWorkAndExpenseRecordController.ACTIVITY_FILTER_ANYGROUP.equals(wc.getFormFields().getActivityFilter()) ? "selected" : ""%> value="<%=CreateWorkAndExpenseRecordController.ACTIVITY_FILTER_ANYGROUP%>" ><%=app.getLabel(CreateWorkAndExpenseRecordController.ACTIVITYTRACKER_CLASS)%> / <%=app.getLabel(CreateWorkAndExpenseRecordController.ACTIVITYCATEGORY_CLASS)%> / <%=app.getLabel(CreateWorkAndExpenseRecordController.ACTIVITYMILESTONE_CLASS)%></option>
<%
																if(Boolean.TRUE.equals(wc.getFormFields().getHasProjects())) {
%>
																	<option <%= CreateWorkAndExpenseRecordController.ACTIVITY_FILTER_PROJECT.equals(wc.getFormFields().getActivityFilter()) ? "selected" : ""%> value="<%=CreateWorkAndExpenseRecordController.ACTIVITY_FILTER_PROJECT%>"	><%=app.getLabel(CreateWorkAndExpenseRecordController.ACTIVITYTRACKER_CLASS)%> [<%=wc.getFieldLabel(CreateWorkAndExpenseRecordController.ACTIVITYTRACKER_CLASS, "userBoolean0", app.getCurrentLocaleAsIndex()) %>]</option>
<%
																}
%>
																<option <%=CreateWorkAndExpenseRecordController.ACTIVITY_FILTER_TRACKER.equals(wc.getFormFields().getActivityFilter()) ? "selected" : ""%> value="<%=CreateWorkAndExpenseRecordController.ACTIVITY_FILTER_TRACKER%>"	><%=app.getLabel(CreateWorkAndExpenseRecordController.ACTIVITYTRACKER_CLASS)%></option>
																<option <%=CreateWorkAndExpenseRecordController.ACTIVITY_FILTER_CATEGORY.equals(wc.getFormFields().getActivityFilter()) ? "selected" : ""%> value="<%=CreateWorkAndExpenseRecordController.ACTIVITY_FILTER_CATEGORY%>" ><%=app.getLabel(CreateWorkAndExpenseRecordController.ACTIVITYCATEGORY_CLASS)%></option>
																<option <%=CreateWorkAndExpenseRecordController.ACTIVITY_FILTER_MILESTONE.equals(wc.getFormFields().getActivityFilter()) ? "selected" : ""%> value="<%=CreateWorkAndExpenseRecordController.ACTIVITY_FILTER_MILESTONE%>"><%=app.getLabel(CreateWorkAndExpenseRecordController.ACTIVITYMILESTONE_CLASS)%></option>
															</select>
														</td>
														<td class="addon">
															<input type="checkbox" id="showActivityGroupNameFilter" name="showActivityGroupNameFilter" <%=Boolean.TRUE.equals(wc.getFormFields().getShowActivityGroupNameFilter()) ? "checked" : ""%> tabindex="<%=tabIndex++%>" onchange="javascript:$('Reload.button').click();" onfocus="<%=ONFOCUS_HANDLER%>" />
														</td>
													</tr>
<%
													tabIndex += 10;
													if (Boolean.TRUE.equals(wc.getFormFields().getHasProjects())) {
														boolean isProject = CreateWorkAndExpenseRecordController.ACTIVITY_FILTER_PROJECT.equals(wc.getFormFields().getActivityFilter());
%>
														<tr <%= isProject ? "" : "style='display:none;'"%>>
															<td class="<%= CssClass.fieldLabel %>">
																<span class="nw"><%=wc.getFieldLabel(CreateWorkAndExpenseRecordController.ACTIVITYTRACKER_CLASS, "userBoolean0", app.getCurrentLocaleAsIndex())%> - <%=wc.getFieldLabel(CreateWorkAndExpenseRecordController.ACTIVITYTRACKER_CLASS, "userString1", app.getCurrentLocaleAsIndex())%>:</span>
															</td>
															<td nowrap>
<%
																if(wc.getSortedActivityGroups() == null || wc.getSortedActivityGroups().isEmpty()) {
																	if (isProject) {
																		activityGroup = null; // reset potentially existing selection
																		wc.appendErrorMsg("no activity groups found!<br>");
																	}
%>
																	<select class="valueL" style="width:50%;float:right;" id="activityFilterXri" name="activityFilterXri" class="valueL" <%=errorStyle%> tabindex="<%=tabIndex+5%>" onfocus="<%=ONFOCUS_HANDLER%>">
																		<option value="">--</option>
																	</select>
<%
																} else {
%>
																	<select class="valueL" style="width:50%;float:right;" id="activityFilterXri" name="activityFilterXri" tabindex="<%=tabIndex+5%>" onchange="javascript:$('resetActivityXri').checked=true;$('Reload.button').click();" onfocus="<%=ONFOCUS_HANDLER%>">
<%
																		boolean hasSelection = false;
																		org.opencrx.kernel.activity1.jmi1.ActivityGroup firstAg = null;
																		for(org.opencrx.kernel.activity1.jmi1.ActivityGroup ag: wc.getSortedActivityGroups()) {
																			if (
																				!EXCLUDE_ACTIVITYTRACKER_TEMPLATES || !(ag instanceof org.opencrx.kernel.activity1.jmi1.ActivityTracker) ||
																				((((org.opencrx.kernel.activity1.jmi1.ActivityTracker)ag).isUserBoolean1() == null) || (!((org.opencrx.kernel.activity1.jmi1.ActivityTracker)ag).isUserBoolean1().booleanValue()))
																			) {
																				boolean selected = false;
																				if (activityFilterXri != null && activityFilterXri.equals(ag.refGetPath().toXRI())) {
																					activityGroup = ag;
																					selected = true;
																					hasSelection = true;
																				}
																				if (firstAg == null) {
																					firstAg = ag;
																				}
%>
																				<option <%=activityFilterXri != null && activityFilterXri.equals(ag.refGetPath().toXRI()) ? "selected" : ""%> value="<%=ag.refGetPath().toXRI()%>"><%=app.getHtmlEncoder().encode(ag.getName(), false)%></option>
<%
																			}
																		}
																		if (!hasSelection) {
																			activityGroup = firstAg; // to ensure proper location of activities
																			activityFilterXri = firstAg.refGetPath().toXRI();
																		}
%>
																	</select>
<%
																}
																if (wc.getSortedProjects() == null || wc.getSortedProjects().isEmpty()) {
																	if (isProject) {
																		wc.appendErrorMsg("no main topics!<br>");
																	}
%>
																	<select id="projectMain" name="projectMain" class="valueL" style="width:49%;float:left;<%=errorStyleInline%>" tabindex="<%=tabIndex++%>" onfocus="<%=ONFOCUS_HANDLER%>">
																		<option value="">--</option>
																	</select>
<%
																} else {
%>
																	<select class="valueL" style="width:49%;float:left;" id="projectMain" name="projectMain" tabindex="<%=tabIndex++%>" onchange="javascript:$('resetActivityXri').checked=true;$('Reload.button').click();" onfocus="<%=ONFOCUS_HANDLER%>">
<%
																		for(org.opencrx.kernel.activity1.jmi1.ActivityTracker at: wc.getSortedProjects()) {
																			if (at.getUserString0() != null) {
%>
																				<option <%=wc.getFormFields().getProjectMain().equals(at.getUserString0().trim()) ? "selected" : ""%> value="<%=app.getHtmlEncoder().encode(at.getUserString0().trim(), false)%>"><%=app.getHtmlEncoder().encode(at.getUserString0().trim(), false)%></option>
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
														tabIndex += 10;
%>
														<tr <%=Boolean.TRUE.equals(wc.getFormFields().getShowActivityGroupNameFilter()) ? "" : "style='display:none;'"%>>
															<td class="<%= CssClass.fieldLabel %>">
																<span class="nw"><%=app.getLabel(CreateWorkAndExpenseRecordController.ACTIVITYFILTERGLOBAL_CLASS)%>:</span>
															</td>
															<td>
																<input type="<%=CreateWorkAndExpenseRecordController.ACTIVITY_FILTER_SEGMENT.equals(wc.getFormFields().getActivityFilter()) ? "hidden" : "text"%>" class="valueL" name="filterActivityGroupName" id="filterActivityGroupName" title="<%=wc.getFieldLabel(CreateWorkAndExpenseRecordController.ACTIVITY_CLASS, "name", app.getCurrentLocaleAsIndex())%> <%=wc.getFieldLabel(CreateWorkAndExpenseRecordController.ACTIVITYGROUPASSIGNMENT_CLASS, "activityGroup", app.getCurrentLocaleAsIndex())%>" <%=CreateWorkAndExpenseRecordController.ACTIVITY_FILTER_SEGMENT.equals(wc.getFormFields().getActivityFilter()) ? "" : "style='width:50%;float:right;'"%> tabindex="<%=tabIndex+5%>" value="<%=app.getHtmlEncoder().encode(wc.getFormFields().getFilterActivityGroupName(), false)%>" onchange="javascript:$('resetActivityXri').checked=true;$('Reload.button').click();" onfocus="<%=ONFOCUS_HANDLER%>" />
																<input type="text" class="valueL" name="filterActivityName" id="filterActivityName" title="<%=wc.getFieldLabel(CreateWorkAndExpenseRecordController.ACTIVITY_CLASS, "name", app.getCurrentLocaleAsIndex())%>" <%=CreateWorkAndExpenseRecordController.ACTIVITY_FILTER_SEGMENT.equals(wc.getFormFields().getActivityFilter()) ? "" : "style='width:49%;float:left;'"%> tabindex="<%=tabIndex++%>" value="<%=app.getHtmlEncoder().encode(wc.getFormFields().getFilterActivityName(), false)%>" onchange="javascript:$('resetActivityXri').checked=true;$('Reload.button').click();" onfocus="<%=ONFOCUS_HANDLER%>" />
															</td>
															<td class="addon"></td>
														</tr>
														<tr>
															<td class="<%= CssClass.fieldLabel %>">
																<div style="float:right;">
																		<img class="timeButtonL" border="0" title=">" alt="" src="../../images/filter_down_star.gif" onclick="javascript:$('activitySortOrder').value = '<%=(wc.getFormFields().getActivitySortOrder() + 1) % MAX_ACTIVITY_SORT_ORDER%>';$('Reload.button').click();" />
																</div>
																<span class="nw"><%=app.getLabel(CreateWorkAndExpenseRecordController.ACTIVITYSEGMENT_CLASS)%>:</span>
															</td>
															<td>
<%
																tabIndex += 10;
																boolean noActivitiesFound = false;
																if (wc.getActivityGroup() != null) {
																	activities = wc.getActivityGroup().getFilteredActivity(activityQuery);
																}
																boolean hasActivitySelection = false;
																String firstActivityXri = null;
%>
																<select id="activityXri" name="activityXri" class="valueL" tabindex="<%=tabIndex++%>" onchange="javascript:$('Reload.button').click();" onfocus="<%=ONFOCUS_HANDLER%>">
<%
																	if (activities == null || activities.isEmpty()) {
																		if ((activityGroup != null) && (activityXri != null && !"MAX".equals(activityXri))) {
																			wc.appendErrorMsg("no activities found!<br>");
																			noActivitiesFound = true;
																		}
%>
																		<option value="">--</option>
<%
																	}
																	else {
																		int activityCounter = 0;
																		int maxToShow = MAX_ACTIVITY_SHOWN_INITIALLY;
																		if (activityXri != null && "MAX".equals(activityXri)) {
																			maxToShow = MAX_ACTIVITY_SHOWN;
																		};
																		for (
																			Iterator i = activities.iterator();
																			i.hasNext() && (activityCounter < maxToShow);
																			activityCounter++
																		) {
																			org.opencrx.kernel.activity1.jmi1.Activity activity = (org.opencrx.kernel.activity1.jmi1.Activity)i.next();
																			boolean selected = activityXri != null && activityXri.equals(activity.refGetPath().toXRI());
																			if (selected) {
																				hasActivitySelection = true;
																			}
																			if (
																				Boolean.TRUE.equals(wc.getFormFields().getShowActivityGroupNameFilter()) &&
																				(activity.getName() != null) && (activity.getName().toUpperCase().indexOf(wc.getFormFields().getFilterActivityName().toUpperCase()) == -1)
																			) {
																				activityCounter--;
																			} else {
																				if (firstActivityXri == null) {
																					firstActivityXri = activity.refGetPath().toXRI();
																				}
%>
																				<option <%=selected ? "selected" : ""%> value="<%=activity.refGetPath().toXRI()%>"><%=openOnly ? "" : (activity.getActivityState() < (short)20 ? "[&ensp;] " : "[X] ")%>#<%=activity.getActivityNumber()%>: <%=app.getHtmlEncoder().encode(activity.getName(), false)%></option>
<%
																			}
																		}
																		if (activityCounter == 0) {
																			wc.appendErrorMsg("no activities found!<br>");
																			noActivitiesFound = true;
%>
																			<option value="">--</option>
<%
																		}
																		if (activityCounter >= maxToShow) {
%>
																			<option value="MAX"><%=activityCounter < MAX_ACTIVITY_SHOWN ? "&mdash;&mdash;&gt;" : "..."%></option>
<%
																		}
																	}
																	if (!hasActivitySelection && !Boolean.TRUE.equals(wc.getFormFields().getResetActivityXri()) && (activityXri != null) && !activityXri.isEmpty() && !"MAX".equalsIgnoreCase(activityXri)) {
																		// add another option to prevent loss of activity selection
																		hasActivitySelection = true;
																		org.opencrx.kernel.activity1.jmi1.Activity activity = (org.opencrx.kernel.activity1.jmi1.Activity)pm.getObjectById(new Path(activityXri));
%>
																		<option selected value="<%=activityXri%>"><%=openOnly ? "" : (activity.getActivityState() < (short)20 ? "[&ensp;] " : "[X] ")%>#<%=activity.getActivityNumber()%>: <%=app.getHtmlEncoder().encode(activity.getName(), false)%></option>
<%
																	}
																	if (!hasActivitySelection) {
																		// set activityXri to first activity in drop down
																		activityXri = firstActivityXri;
																	}
%>
																</select>
															</td>
															<td class="addon">
																	<input type="checkbox" id="excludeClosedActivities" name="excludeClosedActivities" title="Open Activities only" <%=Boolean.TRUE.equals(wc.getFormFields().getExcludeClosedActivities()) ? "checked" : ""%> tabindex="<%=tabIndex++%>" onchange="javascript:$('Reload.button').click();" onfocus="<%=ONFOCUS_HANDLER%>" />
																	<%=noActivitiesFound ? CAUTION : ""%>
															</td>
														</tr>
													</table>
													</fieldset>
					
													<fieldset>
													<table class="fieldGroup">
														<tr>
															<td class="<%= CssClass.fieldLabel %>">
																<span class="nw"><%=wc.getFieldLabel(CreateWorkAndExpenseRecordController.WORKANDEXPENSERECORD_CLASS, "recordType", app.getCurrentLocaleAsIndex())%>:</span>
															</td>
															<td>
																<select class="valueL" name="recordType" id="recordType" tabindex="<%=tabIndex++%>" <%=isWorkRecord ? "onchange='javascript:$(\"Reload.button\").click();'" : ""%> onfocus="<%=ONFOCUS_HANDLER%>">
<%
																	Map<Short,String> recordType_longTextsC = wc.getCodes().getLongTextByCode(isWorkRecord ? featureRecordTypeWork : featureRecordTypeExpense, app.getCurrentLocaleAsIndex(), false);
																	if (recordType_longTextsC == null) {
%>
																		<option value="0">N/A
<%
																	} else {
																		Iterator options = recordType_longTextsC.entrySet().iterator();
																		if (options.hasNext()) {options.next();} // skip N/A
																		while (options.hasNext()) {
																			Map.Entry option = (Map.Entry)options.next();
																			short value = Short.parseShort((option.getKey()).toString());
																			String selectedModifier = wc.getFormFields().getRecordType() == value ? "selected" : "";
																			if (!isWorkRecordInPercent || value == org.opencrx.kernel.backend.Activities.WorkRecordType.STANDARD.getValue()) {
%>
																				<option <%=selectedModifier%> value="<%=value%>"><%=(wc.getCodes().getLongTextByCode(isWorkRecord ? featureRecordTypeWork : featureRecordTypeExpense, app.getCurrentLocaleAsIndex(), true).get(new Short(value)))%>
<%
																			}
																		}
																	}
%>
																</select>
															</td>
															<td class="addon"></td>
														</tr>
<%
														if (isWorkRecordInPercent && activityXri != null && !activityXri.isEmpty()) {
															try {
																name = ((org.opencrx.kernel.activity1.jmi1.Activity)pm.getObjectById(new Path(activityXri))).getName();
															} catch (Exception e) {}
														}
														if (name.length() == 0) {
															wc.appendErrorMsg("name is mandatory!<br>");
														}
%>
														<tr>
															<td class="<%= CssClass.fieldLabel %>">
																<span class="nw"><%=wc.getFieldLabel(CreateWorkAndExpenseRecordController.WORKANDEXPENSERECORD_CLASS, "name", app.getCurrentLocaleAsIndex())%>:</span>
															</td>
															<td>
																<input type="text" class="valueL <%=isWorkRecordInPercent ? "" : "mandatory"%>" name="name" id="name" <%=isWorkRecordInPercent ? "readonly" : " "%> tabindex="<%=tabIndex++%>" value="<%=app.getHtmlEncoder().encode(name, false)%>" <%=name.length() == 0 ? errorStyle : ""%> onfocus="<%=ONFOCUS_HANDLER%>" />
															</td>
															<td class="addon">
																	<%= name.length() == 0 ? CAUTION : ""%>
															</td>
														</tr>
														<tr>
															<td class="<%= CssClass.fieldLabel %>">
																<span class="nw"><%=wc.getFieldLabel(CreateWorkAndExpenseRecordController.WORKANDEXPENSERECORD_CLASS, "description", app.getCurrentLocaleAsIndex())%>:</span>
															</td>
															<td>
																<textarea rows="4" class="valueL" name="description" id="description" tabindex="<%=tabIndex++%>" onfocus="<%=ONFOCUS_HANDLER%>"><%=app.getHtmlEncoder().encode(wc.getFormFields().getDescription(), false) %></textarea>
															</td>
															<td class="addon"></td>
														</tr>
														<tr <%=isWorkRecordInPercent ? "style='display:none;'" : ""%>>
															<td class="<%= CssClass.fieldLabel %>">
																<span class="nw"><%=wc.getFieldLabel(CreateWorkAndExpenseRecordController.WORKANDEXPENSERECORD_CLASS, "rate", app.getCurrentLocaleAsIndex())%>:</span>
															</td>
															<td nowrap>
																<input type="text" name="rate" id="rate" <%=isWorkRecord ? "" : "class='mandatory'"%> style="font-weight:bold;width:47%;float:right;padding-top:2px;padding-right:2px;text-align:right;<%=!isWorkRecord && wc.getParaRate() == null ? errorStyleInline: ""%>" tabindex="<%=tabIndex+5%>" value="<%=wc.getResourceRate()%>" onfocus="<%=ONFOCUS_HANDLER%>" />
																<select class="valueL" style="width:49%;float:left;" id="billingCurrency" name="billingCurrency" tabindex="<%=tabIndex++%>" onfocus="<%=ONFOCUS_HANDLER%>">
<%
																	Map<Short,String> billingCurrency_longTextsC = wc.getCodes().getLongTextByCode(featureBillingCurrency, app.getCurrentLocaleAsIndex(), false);
																	if (billingCurrency_longTextsC == null) {
%>
																		<option value="0">N/A
<%
																	} else {
																		for(Iterator options = billingCurrency_longTextsC.entrySet().iterator() ; options.hasNext(); ) {
																			Map.Entry option = (Map.Entry)options.next();
																			short value = Short.parseShort((option.getKey()).toString());
																			String selectedModifier = billingCurrency == value ? "selected" : "";
%>
																			<option <%=selectedModifier%> value="<%=value%>"><%=(wc.getCodes().getLongTextByCode(featureBillingCurrency, app.getCurrentLocaleAsIndex(), true).get(new Short(value)))%>
<%
																		}
																	}
																	tabIndex += 10;
%>
																</select>
																<input type="hidden" name="previousRecordType" value="<%=Short.toString(wc.getFormFields().getRecordType())%>" />
															</td>
															<td class="addon"></td>
														</tr>
														<tr <%=isWorkRecordInPercent ? "style='display:none;'" : ""%>>
															<td class="<%= CssClass.fieldLabel %>">
																<span class="nw"><%=wc.getFieldLabel(CreateWorkAndExpenseRecordController.WORKANDEXPENSERECORD_CLASS, "isBillable", app.getCurrentLocaleAsIndex())%>:</span>
															</td>
															<td>
																<input type="checkbox" id="isBillable" name="isBillable" <%=Boolean.TRUE.equals(wc.getFormFields().getIsBillable()) ? "checked" : ""%> tabindex="<%=tabIndex++%>" onfocus="<%=ONFOCUS_HANDLER%>" />
															</td>
															<td class="addon"></td>
														</tr>
														<tr class="time">
															<td class="timelabel">
																<span class="nw"><b><%=wc.getDateFormat().format(wc.getDateAsCalendar(selectedDateStr, app).getTime())%></b>:</span>
															</td>
															<td class="time">
																<table>
																	<tr class="centered" <%=isWorkRecordInPercent ? "style='display:none;'" : ""%>>
																		<td><%=wc.getFieldLabel(CreateWorkAndExpenseRecordController.WORKANDEXPENSERECORD_CLASS, "startedAt", app.getCurrentLocaleAsIndex())%></td>
																		<td></td>
																		<td><%=wc.getFieldLabel(CreateWorkAndExpenseRecordController.WORKANDEXPENSERECORD_CLASS, "endedAt", app.getCurrentLocaleAsIndex())%></td>
																		<td style="width:100%;">&nbsp;</td>
																	</tr>
																	<tr class="centered" <%=isWorkRecordInPercent ? "style='display:none;'" : ""%>>
																		<td>
																			<img class="timeButtonL" border="0" title="- 0:15" alt="" src="../../images/arrow_smallleft.gif" onclick="javascript:var hh_mm = timeTick($('startedAtHH').value + ':' + $('startedAtMM').value, -15);$('startedAtHH').value = hh_mm.split(':')[0];$('startedAtMM').value = hh_mm.split(':')[1];" /><input type="text" class="time" name="startedAtHH" id="startedAtHH" tabindex="<%=tabIndex++%>" value="<%=wc.getFormFields().getStartedAtHH()%>" <%=wc.getStartedAt() == null ? errorStyle : ""%> onkeypress="javascript:oldValue=this.value;" onkeyup="javascript:positiveDecimalsVerify(this);" onfocus="<%=ONFOCUS_HANDLER%>" />:<input type="text" class="time" name="startedAtMM" id="startedAtMM"" tabindex="<%=tabIndex++%>" value="<%=wc.getFormFields().getStartedAtMM()%>" <%=wc.getStartedAt() == null ? errorStyle : ""%> onkeypress="javascript:oldValue=this.value;" onkeyup="javascript:positiveDecimalsVerify(this);" onfocus="<%=ONFOCUS_HANDLER%>" /><img class="timeButtonR" border="0" title="+ 0:15" alt="" src="../../images/arrow_smallright.gif" onclick="javascript:var hh_mm = timeTick($('startedAtHH').value + ':' + $('startedAtMM').value, +15);$('startedAtHH').value = hh_mm.split(':')[0];$('startedAtMM').value = hh_mm.split(':')[1];" />
																		</td>
																		<td>&mdash;</td>
																		<td>
																			<img class="timeButtonL" border="0" title="- 0:15" alt="" src="../../images/arrow_smallleft.gif" onclick="javascript:var hh_mm = timeTick($('endedAtHH').value + ':' + $('endedAtMM').value, -15);$('endedAtHH').value = hh_mm.split(':')[0];$('endedAtMM').value = hh_mm.split(':')[1];" /><input type="text" class="time" name="endedAtHH" id="endedAtHH" tabindex="<%=tabIndex++%>" value="<%=wc.getFormFields().getEndedAtHH()%>" onkeypress="javascript:oldValue=this.value;" onkeyup="javascript:positiveDecimalsVerify(this);" onfocus="<%=ONFOCUS_HANDLER%>" />:<input type="text" class="time" name="endedAtMM" id="endedAtMM" tabindex="<%=tabIndex++%>" value="<%=wc.getFormFields().getEndedAtMM()%>" onkeypress="javascript:oldValue=this.value;" onkeyup="javascript:positiveDecimalsVerify(this);" onfocus="<%=ONFOCUS_HANDLER%>" /><img class="timeButtonR" border="0" title="+ 0:15" alt="" src="../../images/arrow_smallright.gif" onclick="javascript:var hh_mm = timeTick($('endedAtHH').value + ':' + $('endedAtMM').value, +15);$('endedAtHH').value = hh_mm.split(':')[0];$('endedAtMM').value = hh_mm.split(':')[1];" />
																		</td>
																		<td></td>
																	</tr>
					
																	<!--	WorkRecord -->
																	<tr class="centered" <%=isWorkRecord && !isWorkRecordInPercent ? "" : "style='display:none;'"%>>
																		<td style="padding-top:5px;">hh:mm</td>
																		<td></td>
																		<td></td>
																		<td></td>
																	</tr>
																	<tr class="centered" <%=isWorkRecord && !isWorkRecordInPercent ? "" : "style='display:none;'"%>>
																		<td>
																			<img class="timeButtonL" border="0" title="- 0:15" alt="" src="../../images/arrow_smallleft.gif" onclick="javascript:var hh_mm = timeTick($('effortHH').value + ':' + $('effortMM').value, -15);$('effortHH').value = hh_mm.split(':')[0];$('effortMM').value = hh_mm.split(':')[1];" /><input type="text" class="time" name="effortHH" id="effortHH" tabindex="<%=tabIndex++%>" value="<%=wc.getFormFields().getEffortHH()%>" <%=wc.getParaEffortHH() == null ? errorStyle : ""%> onkeypress="javascript:oldValue=this.value;" onkeyup="javascript:positiveDecimalsVerify(this);" onfocus="<%=ONFOCUS_HANDLER%>" />:<input type="text" class="time" name="effortMM" id="effortMM" tabindex="<%=tabIndex++%>" value="<%=wc.getFormFields().getEffortMM()%>" <%=wc.getParaEffortMM() == null ? errorStyle : ""%> onkeypress="javascript:oldValue=this.value;" onkeyup="javascript:positiveDecimalsVerify(this);" onfocus="<%=ONFOCUS_HANDLER%>" /><img class="timeButtonR" border="0" title="+ 0:15" alt="" src="../../images/arrow_smallright.gif" onclick="javascript:var hh_mm = timeTick($('effortHH').value + ':' + $('effortMM').value, +15);$('effortHH').value = hh_mm.split(':')[0];$('effortMM').value = hh_mm.split(':')[1];" />
																		</td>
																		<td></td>
																		<td></td>
																		<td></td>
																	</tr>
																	<!--	WorkRecordInPercent -->
																	<tr <%=isWorkRecordInPercent ? "" : "style='display:none;'"%>>
																		<td style="vertical-align:bottom;">
																			<img class="timeButtonL" border="0" title="- 5%" alt="" src="../../images/arrow_smallleft.gif" onclick="javascript:$('quantPercent').value = percentageTick($('quantPercent').value, -5);" /><input type="text" class="percentage" name="quantPercent" id="quantPercent" tabindex="<%=tabIndex++%>" value="<%= wc.getFormFields().getQuantPercent() %>" <%= wc.getFormFields().getQuantPercent() == null || wc.getFormFields().getQuantPercent().isEmpty() ? errorStyle : ""%> onkeypress="javascript:oldValue=this.value;" onkeyup="javascript:positiveDecimalsVerify(this);" onfocus="<%=ONFOCUS_HANDLER%>" />%<img class="timeButtonR" border="0" title="+ 5%" alt="" src="../../images/arrow_smallright.gif" onclick="javascript:$('quantPercent').value = percentageTick($('quantPercent').value, 5);" />
																		</td>
																		<td style="vertical-align:bottom;width:30px;">&nbsp;</td>
																		<td style="vertical-align:bottom;padding-right:30px;">
																				<%=wc.getFieldLabel(CreateWorkAndExpenseRecordController.RESOURCE_CLASS, "calendar", app.getCurrentLocaleAsIndex())%>:<br>
																				<%=wc.getResourceCalendar() != null && wc.getResourceCalendar().getName() != null ? wc.getResourceCalendar().getName() : "--"%>
																		</td>
																		<td style="vertical-align:bottom;padding-right:30px;">
																			<%=wc.getResourceCalendarDay() != null && wc.getResourceCalendarDay().getName() != null ? app.getLabel(CreateWorkAndExpenseRecordController.CALENDARDAY_CLASS) + ":  " + wc.getResourceCalendarDay().getName() : "Default Load: " + wc.getResourceDefaultLoad() + "%"%><br>
<%
																			if(wc.getResourceCalendar() != null && wc.getResourceCalendarDay() != null && wc.getResourceCalendarDay().getName() != null) {
																				// Change options
																				for(Integer load: Arrays.asList(0, 25, 50, 75, 100)) {
%>
																					<input type="submit" name="updateCalendarDay<%=load%>" tabindex="<%=tabIndex++%>" value="<%=load%>%" onclick="javascript:$('updateCalendarDay').value='<%=wc.getResourceCalendarDayName()+"@"+load %>';$('Reload.button').click();" style="font-size:10px;font-weight:bold;width:4em;<%=wc.getResourceCalendarDayLoad() != null && wc.getResourceCalendarDayLoad().equals(load) ? "border:2px black solid;" : ""%>" />
<%
																				}
																			} else if (wc.getResourceCalendar() != null) {
																				// Create options
																				for(Integer load: Arrays.asList(0, 25, 50, 75, 100)) {
%>
																					<input type="submit" name="createCalendarDay<%=load%>" tabindex="<%=tabIndex++%>" value="<%=load%>%" onclick="javascript:$('createCalendarDay').value='<%=wc.getResourceCalendarDayName()+"@"+load %>';$('Reload.button').click();" style="font-size:10px;font-weight:bold;width:4em;<%=wc.getResourceDefaultLoad().equals(load) ? "border:2px black solid;" : ""%>" />
<%
																				}
																			}
%>
																		</td>
																	</tr>
																	<!--	ExpenseRecord -->
																	<tr class="centered" <%=isWorkRecord ? "style='display:none;'" : ""%>>
																		<td style="padding-top:5px;"><%=wc.getFieldLabel(CreateWorkAndExpenseRecordController.WORKANDEXPENSERECORD_CLASS, "quantity", app.getCurrentLocaleAsIndex())%></td>
																		<td></td>
																		<td style="padding-top:5px;"><%=wc.getFieldLabel(CreateWorkAndExpenseRecordController.WORKANDEXPENSERECORD_CLASS, "quantityUom", app.getCurrentLocaleAsIndex())%></td>
																		<td></td>
																	</tr>
																	<tr class="centered" <%=isWorkRecord ? "style='display:none;'" : ""%>>
																		<td>
																			<input type="text" class="quantity" <%=Boolean.TRUE.equals(wc.getQuantityIsZero()) ? errorStyle : ""%> name="quantity" id="quantity" tabindex="<%=tabIndex++%>" value="<%=wc.getFormFields().getQuantity()%>" <%=wc.getParaQuantity() == null ? errorStyle : ""%> onkeypress="javascript:oldValue=this.value;" onkeyup="javascript:positiveDecimalsVerify(this);" onchange="javascript:$('Reload.button').click();" onfocus="<%=ONFOCUS_HANDLER%>" />
																		</td>
																		<td></td>
																		<td colspan="2">
<%
																			boolean noUomsFound = false;
																			org.opencrx.kernel.uom1.jmi1.Segment uomSegment = (org.opencrx.kernel.uom1.jmi1.Segment)pm.getObjectById(
																				new Path("xri://@openmdx*org.opencrx.kernel.uom1").getDescendant("provider", wc.getProviderName(), "segment", "Root")
																			);
																			org.opencrx.kernel.uom1.cci2.UomQuery uomFilter = (org.opencrx.kernel.uom1.cci2.UomQuery)pm.newQuery(org.opencrx.kernel.uom1.jmi1.Uom.class);
																			uomFilter.name().elementOf(Arrays.asList(CreateWorkAndExpenseRecordController.UOM_NAMES));
																			uomFilter.orderByName().ascending();
																			uomFilter.orderByDescription().ascending();
																			List uoms = uomSegment.getUom(uomFilter);
																			if (uoms.isEmpty()) {
																				wc.appendErrorMsg("no matching UOMs found!<br>");
																				noUomsFound = true;
%>
																				<select id="uomXri" name="uomXri" class="valueL" <%=errorStyle%> tabindex="<%=tabIndex++%>" onfocus="<%=ONFOCUS_HANDLER%>">
																					<option value="">--</option>
																				</select>
<%
																			} else {
%>
																				<select class="valueL" id="uomXri" name="uomXri" tabindex="<%=tabIndex++%>" onfocus="<%=ONFOCUS_HANDLER%>">
<%
																					for (
																						Iterator i = uoms.iterator();
																						i.hasNext();
																					) {
																						org.opencrx.kernel.uom1.jmi1.Uom uom = (org.opencrx.kernel.uom1.jmi1.Uom)i.next();
%>
																						<option <%=(wc.getFormFields().getUomXri() != null) && (wc.getFormFields().getUomXri().equals(uom.refGetPath().toXRI())) ? "selected" : ""%> value="<%=uom.refGetPath().toXRI()%>"><%=app.getHtmlEncoder().encode(uom.getName(), false)%> [<%=uom.getDescription() != null ? app.getHtmlEncoder().encode(uom.getDescription(), false) : "--"%>]</option>
<%
																					}
%>
																				</select>
<%
																			}
%>
																		</td>
																	</tr>
																	<tr class="centered" <%=isWorkRecord ? "style='display:none;'" : ""%>>
																		<td style="padding-top:5px;"><%=wc.getFieldLabel(CreateWorkAndExpenseRecordController.WORKANDEXPENSERECORD_CLASS, "isReimbursable", app.getCurrentLocaleAsIndex())%></td>
																		<td></td>
																		<td style="padding-top:5px;"><%=wc.getFieldLabel(CreateWorkAndExpenseRecordController.WORKANDEXPENSERECORD_CLASS, "paymentType", app.getCurrentLocaleAsIndex())%></td>
																		<td></td>
																	</tr>
																	<tr class="centered" <%=isWorkRecord ? "style='display:none;'" : ""%>>
																		<td>
																			<input type="checkbox" id="isReimbursable" name="isReimbursable" <%=Boolean.TRUE.equals(wc.getFormFields().getIsReimbursable()) ? "checked" : ""%> tabindex="<%=tabIndex++%>" onfocus="<%=ONFOCUS_HANDLER%>" />
																		</td>
																		<td></td>
																		<td colspan="2">
																			<select class="valueL" id="paymentType" name="paymentType" tabindex="<%=tabIndex++%>" onfocus="<%=ONFOCUS_HANDLER%>">
<%
																				Map<Short,String> paymentType_longTextsC = wc.getCodes().getLongTextByCode(featurePaymentType, app.getCurrentLocaleAsIndex(), false);
																				if (paymentType_longTextsC == null) {
%>
																					<option value="0">N/A
<%
																				} else {
																					for(Iterator options = paymentType_longTextsC.entrySet().iterator() ; options.hasNext(); ) {
																						Map.Entry option = (Map.Entry)options.next();
																						short value = Short.parseShort((option.getKey()).toString());
																						String selectedModifier = wc.getFormFields().getPaymentType() == value ? "selected" : "";
%>
																						<option <%=selectedModifier%> value="<%=value%>"><%=(wc.getCodes().getLongTextByCode(featurePaymentType, app.getCurrentLocaleAsIndex(), true).get(new Short(value)))%>
<%
																					}
																				}
%>		
																			</select>
																		</td>
																	</tr>
					
																</table>
															</td>
															<td class="addon"></td>
														</tr>
													</table>
													</fieldset>
													<fieldset <%=Boolean.TRUE.equals(wc.getShowMakePrivate()) ? "" : "style='display:none;'"%>>
													<table class="fieldGroup">
														<tr>
															<td class="<%= CssClass.fieldLabel %>" style="padding-top:5px;">
																<span class="nw"><strong><%=CreateWorkAndExpenseRecordController.PRIVATE_TOKEN%></strong>:</span>
															</td>
															<td>
																<input type="checkbox" id="makePrivate" name="makePrivate" <%=Boolean.TRUE.equals(makePrivate) ? "checked" : ""%> tabindex="<%=tabIndex++%>" onfocus="<%=ONFOCUS_HANDLER%>" />
																<%=wc.getGroupNames()%>
															</td>
															<td class="addon"></td>
														</tr>
													</table>
													</fieldset>
												</td>
											</tr>
											<tr>
												<td>
<%
												if (!Boolean.TRUE.equals(wc.getCanExecuteAdd()) || Boolean.TRUE.equals(wc.getCreationFailed()) || noActivitiesFound || noResourcesFound) {
%>
													<div style="float:right;" title="<%=wc.getErrorMsg().replace("<br>", " - ")%>" >
														<%=CAUTION%>
													</div>
<%
												}
%>
												<div class="buttons">
													<input type="submit" id="EvictReload.Button" name="EvictReload" class="<%= CssClass.btn.toString() %> <%= CssClass.btn_light.toString() %>"  tabindex="<%=tabIndex++%>" value="<%=app.getTexts().getReloadText()%>" onclick="javascript:$('Command').value=this.name;" />
												</div>
											</td>
											<td></td>
											<td>
												<input type="submit" id="AddWorkRecord.button" name="AddWorkRecord" <%=noActivitiesFound || noResourcesFound ? "disabled" : ""%> class="<%= CssClass.btn.toString() %> <%= CssClass.btn_light.toString() %>" tabindex="<%=tabIndex++%>" value="<%=app.getTexts().getNewText()%>" onclick="javascript:$('Command').value=this.name;" />
												<input type="submit" id="Cancel.button" name="Cancel" class="<%= CssClass.btn.toString() %> <%= CssClass.btn_light.toString() %>" tabindex="<%=tabIndex++%>" value="<%=app.getTexts().getCloseText()%>" onclick="javascript:$('Command').value=this.name;" />
											</td>
										</tr>
									</table>
<%
									if (SHOW_ERRORS && !wc.getErrorMsg().isEmpty()) {
%>
										<div style="background-color:red;color:white;border:1px solid black;padding:10px;font-weight:bold;margin-top:10px;">
											<%=wc.getErrorMsg()%>
										</div>
<%
									}
%>
									<br>
<%
									if ((wc.getContact() != null) || (wc.getResource() != null)) {
%>
										<hr>
										<h2 style="padding-left:5px;">
											<%=app.getLabel(CreateWorkAndExpenseRecordController.WORKANDEXPENSERECORD_CLASS)%>
											<input type="checkbox" name="isFullMonth" <%=Boolean.TRUE.equals(wc.getFormFields().getIsFullMonth()) ? "checked" : ""%> tabindex="<%=tabIndex++%>" onchange="javascript:$('Reload.button').click();" />
											[<%=wc.getCalendarBeginOfPeriod() != null ? wc.getDateFormat().format(wc.getCalendarBeginOfPeriod().getTime()) : "--"%> &mdash; <%=wc.getCalendarEndOfPeriod() != null ? wc.getDateFormat().format(wc.getCalendarEndOfPeriod().getTime()) : "--"%>]
										</h2>
<%
										if (wc.getWorkAndExpenseRecords() != null && !wc.getWorkAndExpenseRecords().isEmpty()) {
											boolean showFullStartedAtDate = !isWorkRecordInPercent && Boolean.TRUE.equals(wc.getFormFields().getIsFullStartedAtDate());
%>
											<table><tr><td style="padding-left:5px;">
												<table class="gridTable">
													<tr class="gridTableHeader">
														<td class="smallheaderR" colspan="2">
															<%=wc.getFieldLabel(CreateWorkAndExpenseRecordController.WORKANDEXPENSERECORD_CLASS, "startedAt", app.getCurrentLocaleAsIndex())%>
															<input type="checkbox" name="isFullStartedAtDate" <%=showFullStartedAtDate ? "checked" : ""%> <%=isWorkRecordInPercent ? "style='display:none;'" : ""%> tabindex="<%=tabIndex++%>" onchange="javascript:$('Reload.button').click();" />
														</td>
														<td class="smallheaderR <%=showFullStartedAtDate ? "" : "hidden"%>" colspan="2"><%=wc.getFieldLabel(CreateWorkAndExpenseRecordController.WORKANDEXPENSERECORD_CLASS, "endedAt", app.getCurrentLocaleAsIndex())%></td>
														<td class="smallheaderR"><%=isWorkRecord ? (isWorkRecordInPercent ? "%" : "hh:mm") : wc.getFieldLabel(CreateWorkAndExpenseRecordController.WORKANDEXPENSERECORD_CLASS, "quantity", app.getCurrentLocaleAsIndex())%></td>
														<td class="smallheaderR"><%=isWorkRecordInPercent ? "&sum;" : ""%></td>
<%
														if (!isWorkRecord) {
%>
															<td class="smallheader"><%=wc.getFieldLabel(CreateWorkAndExpenseRecordController.WORKANDEXPENSERECORD_CLASS, "quantityUom", app.getCurrentLocaleAsIndex())%>&nbsp;</td>
<%
														}
														if (isWorkRecord && !isWorkRecordInPercent) {
%>
															<td class="smallheader">&nbsp;</td>
															<td class="smallheaderR" colspan="2"><%=wc.getFieldLabel(CreateWorkAndExpenseRecordController.WORKANDEXPENSERECORD_CLASS, "billableAmount", app.getCurrentLocaleAsIndex())%></td>
															<td class="smallheaderR" title="<%=wc.getFieldLabel(CreateWorkAndExpenseRecordController.WORKANDEXPENSERECORD_CLASS, "isBillable", app.getCurrentLocaleAsIndex())%>">$&nbsp;</td>
															<td class="smallheaderR" title="<%=wc.getFieldLabel(CreateWorkAndExpenseRecordController.WORKANDEXPENSERECORD_CLASS, "isReimbursable", app.getCurrentLocaleAsIndex())%>">*&nbsp;</td>
<%
														}
%>
														<td class="smallheader"><%=wc.getFieldLabel(CreateWorkAndExpenseRecordController.WORKANDEXPENSERECORD_CLASS, "name", app.getCurrentLocaleAsIndex())%>&nbsp;</td>
														<td class="smallheader"><%=wc.getFieldLabel(CreateWorkAndExpenseRecordController.WORKANDEXPENSERECORD_CLASS, "activity", app.getCurrentLocaleAsIndex())%>&nbsp;</td>
														<td class="smallheader" <%=isWorkRecordInPercent ? "style='display:none;'" : ""%>><%=wc.getFieldLabel(CreateWorkAndExpenseRecordController.WORKANDEXPENSERECORD_CLASS, "recordType", app.getCurrentLocaleAsIndex())%>&nbsp;</td>
														<td class="smallheader"><%=wc.getFieldLabel(CreateWorkAndExpenseRecordController.WORKANDEXPENSERECORD_CLASS, "description", app.getCurrentLocaleAsIndex())%>&nbsp;</td>
														<td class="smallheader">&nbsp;</td>
													</tr>
<%
													boolean isEvenRow = false;
													boolean isFirstRow = false;
													java.math.BigDecimal dailySum = java.math.BigDecimal.ZERO;
													String startedAtCurrent = "";
													String startedAtPrevious = "";
													int rowCounter = 0;
													for (
														Iterator w = wc.getWorkAndExpenseRecords().values().iterator();
														w.hasNext();
													) {
														org.opencrx.kernel.activity1.jmi1.WorkAndExpenseRecord workAndExpenseRecord = (org.opencrx.kernel.activity1.jmi1.WorkAndExpenseRecord)w.next();
														GregorianCalendar startedAtDate = new GregorianCalendar(app.getCurrentLocale());
														startedAtDate.setMinimalDaysInFirstWeek(4); // this conforms to DIN 1355/ISO 8601
														startedAtDate.setTime(workAndExpenseRecord.getStartedAt());
														startedAtCurrent = wc.getDateAsString(startedAtDate);
														boolean matchWithFormStartedAt = startedAtCurrent.compareTo(selectedDateStr) == 0;
														GregorianCalendar creationDate = new GregorianCalendar(app.getCurrentLocale());
														creationDate.setTimeZone(TimeZone.getTimeZone(app.getCurrentTimeZone()));
														creationDate.setMinimalDaysInFirstWeek(4); // this conforms to DIN 1355/ISO 8601
														creationDate.setTime(workAndExpenseRecord.getCreatedAt());
														boolean matchWithFormJustCreated = workAndExpenseRecord.refGetPath().toXRI().equals(wc.getFormFields().getLastCreatedWorkExpenseRecordXri());
														org.opencrx.kernel.activity1.jmi1.Activity activity = workAndExpenseRecord.getActivity();
														String recordHref = "";
														Action action = new Action(
															org.openmdx.portal.servlet.action.SelectObjectAction.EVENT_ID,
															new Action.Parameter[]{
																new Action.Parameter(Action.PARAMETER_OBJECTXRI, workAndExpenseRecord.refMofId())
															},
															"",
															true // enabled
														);
														recordHref = "../../" + action.getEncodedHRef();
														String activityHref = "";
														if (activity != null) {
															action = new Action(
																org.openmdx.portal.servlet.action.SelectObjectAction.EVENT_ID,
																new Action.Parameter[]{
																		new Action.Parameter(Action.PARAMETER_OBJECTXRI, activity.refMofId())
																},
																"",
																true // enabled
															);
															activityHref = "../../" + action.getEncodedHRef();
														}
														boolean isDayBreak = false;
														if (isWorkRecordInPercent && startedAtCurrent.compareTo(startedAtPrevious) != 0) {
															// new day --> break and print sum
															isDayBreak = true;
															startedAtPrevious = startedAtCurrent;
															dailySum = java.math.BigDecimal.ZERO;
														}
														if (isFirstRow) {
															isDayBreak = false;
															isFirstRow = false;
														}
														double recordTotal = 0.0;
														boolean quantityError = false;
														try {
															if (workAndExpenseRecord.getQuantity() != null && workAndExpenseRecord.getRate() != null) {
																dailySum = dailySum.add(workAndExpenseRecord.getQuantity());
																recordTotal = workAndExpenseRecord.getQuantity().doubleValue() * workAndExpenseRecord.getRate().doubleValue();
															}
														} catch (Exception e) {
															quantityError = true;
														}
														if (workAndExpenseRecord.getBillingCurrency() == 0) {
															quantityError = true;
														}
														String currency = "N/A";
														try {
																currency = (String)(wc.getCodes().getShortText(featureBillingCurrency, app.getCurrentLocaleAsIndex(), true, true).get(new Short(workAndExpenseRecord.getBillingCurrency())));
														} catch (Exception e) {}
%>
														<tr <%= matchWithFormJustCreated ? "class='created'" : (matchWithFormStartedAt ? "class='match'" : (isEvenRow ? "class='even'" : "")) %>>
															<td><a href='<%= recordHref %>' target='_blank'><%= workAndExpenseRecord.getStartedAt() != null ? wc.getWeekDayFormat().format(workAndExpenseRecord.getStartedAt()) : "--" %>&nbsp;</a></td>
															<td class="padded_r"><a href='<%= recordHref %>' target='_blank'><%= workAndExpenseRecord.getStartedAt() != null ? (showFullStartedAtDate ? wc.getDateTimeFormat().format(workAndExpenseRecord.getStartedAt()) : wc.getDateOnlyFormat().format(workAndExpenseRecord.getStartedAt())) : "--" %></a></td>
															<td <%= showFullStartedAtDate ? "" : "class='hidden'" %>><a href='<%= recordHref %>' target='_blank'><%= workAndExpenseRecord.getEndedAt() != null ? wc.getWeekDayFormat().format(workAndExpenseRecord.getEndedAt()) : "--" %>&nbsp;</a></td>
															<td class="padded_r <%= showFullStartedAtDate ? "" : "hidden" %>"><a href='<%= recordHref %>' target='_blank'><%= workAndExpenseRecord.getEndedAt() != null ? wc.getDateTimeFormat().format(workAndExpenseRecord.getEndedAt()) : "--" %></a></td>
															<td class="padded_r"><a href='<%= recordHref %>' target='_blank'><%=	workAndExpenseRecord.getQuantity() == null ? "--" : (isWorkRecord ? (isWorkRecordInPercent ? wc.getFormatter0().format(workAndExpenseRecord.getQuantity()) : wc.decimalMinutesToHhMm(workAndExpenseRecord.getQuantity().doubleValue() * 60.0)) : wc.getQuantityFormat().format(workAndExpenseRecord.getQuantity())) %></a></td>
<%
															if (isWorkRecordInPercent) {
%>
																<td class="padded_r <%= dailySum.doubleValue() == 100.0 ? "" : "error" %> " id="cumSum<%= rowCounter++ %>"><%= wc.getFormatter0().format(dailySum.doubleValue()) %></td>
<%
															} else {
%>
																<td class="padded_r"></td>
<%
															}
															if (!isWorkRecord) {
%>
																<td class="padded"><a href='<%= recordHref %>' target='_blank'><%= workAndExpenseRecord.getQuantityUom() != null && workAndExpenseRecord.getQuantityUom().getName() != null ? app.getHtmlEncoder().encode(workAndExpenseRecord.getQuantityUom().getName(), false) : "?" %>&nbsp;</a></td>
<%
															}
															if (isWorkRecord && !isWorkRecordInPercent) {
%>
																<td class="padded_r"><a href='<%= recordHref %>' target='_blank'>[<%= workAndExpenseRecord.getRate() != null ? wc.getRatesEpFormat().format(workAndExpenseRecord.getRate()) : "--" %>]&nbsp;</a></td>
																<td class="padded"	 <%= quantityError ? errorStyle : "" %>><a href='<%= recordHref %>' target='_blank'><%= currency %></a></td>
																<td class="padded_r" <%= quantityError ? errorStyle : "" %>><a href='<%= recordHref %>' target='_blank'><%= wc.getRatesEpFormat().format(recordTotal) %></a></td>
																<td class="padded"><a href='<%= recordHref %>' target='_blank'><img src="../../images/<%= Boolean.TRUE.equals(workAndExpenseRecord.isBillable()) ? "" : "not" %>checked_r.gif" /></a></td>
																<td class="padded"><a href='<%= recordHref %>' target='_blank'><img src="../../images/<%= Boolean.TRUE.equals(workAndExpenseRecord.isReimbursable()) ? "" : "not" %>checked_r.gif" /></a></td>
<%
															}
%>
															<td class="padded"><a href='<%= recordHref %>' target='_blank'><%= app.getHtmlEncoder().encode(workAndExpenseRecord.getName(), false) %></a></td>
															<td class="padded"><a href='<%= activityHref %>' target='_blank'>#<%= app.getHtmlEncoder().encode(new ObjectReference(activity, app).getTitle(), false) %>&nbsp;</a></td>
															<td class="padded" <%= isWorkRecordInPercent ? "style='display:none;'" : "" %>><a href='<%= recordHref %>' target='_blank'><%= (wc.getCodes().getLongTextByCode(featureRecordType, app.getCurrentLocaleAsIndex(), true).get(new Short(workAndExpenseRecord.getRecordType()))) %></a></td>
															<td class="padded"><%= workAndExpenseRecord.getDescription() != null ? app.getHtmlEncoder().encode(workAndExpenseRecord.getDescription().replace("\n", "<br />"), false) : "" %></td>
															<td class="padded">
																<img src="../../images/deletesmall.gif" style="cursor:pointer;" onclick="javascript:$('deleteWorkRecordXri').value='<%= app.getHtmlEncoder().encode(workAndExpenseRecord.refGetPath().toXRI(), false) %>';$('Command').value='DeleteWorkRecord';$('Reload.button').click();" />
<%
																if (!isDayBreak) {
%>
																	<script language="javascript" type="text/javascript">
																			try {
																					$('cumSum<%= rowCounter-2 %>').innerHTML = '';
																			} catch (e) {}
																	</script>
<%
																}
%>
															</td>
														</tr>
<%
														isEvenRow = !isEvenRow;
													}
%>
												</table>
											</td></tr></table>
<%
											if (isWorkRecord & !isWorkRecordInPercent) {
												GregorianCalendar calendarBeginOfWeek = wc.getCalendarBeginOfPeriod();
%>
												<table><tr><td style="padding-left:5px;">
												<table class="gridTable">
													<tr class="gridTableHeader">
<%
														int dayCounter = 0;
														for (int i = calendarBeginOfWeek.get(GregorianCalendar.DAY_OF_WEEK); dayCounter < 7; dayCounter++) {
%>
															<td class="smallheader"><%= wc.getWeekDayFormat().format(calendarBeginOfWeek.getTime()) %></td>
<%
															calendarBeginOfWeek.add(GregorianCalendar.DAY_OF_MONTH, 1);
														}
%>
														<td class="smallheader">hh:mm&nbsp;&nbsp;</td>
														<td class="smallheaderR">&nbsp;</td>
													</tr>
													<tr>
<%
														double sumWeek = 0.0;
														dayCounter = 0;
														for(int i = calendarBeginOfWeek.getFirstDayOfWeek(); dayCounter < 7; dayCounter++) {
%>
															<td class="padded_r"><%= isWorkRecordInPercent ? wc.getFormatter0().format(wc.getSumDays()[i % 7])+"%" : wc.decimalMinutesToHhMm(wc.getSumDays()[i % 7] * 60.0) %></td>
<%
															sumWeek += wc.getSumDays()[i % 7];
															i++;
														}
%>
														<td class="padded_r"><%= isWorkRecordInPercent ? "" : wc.decimalMinutesToHhMm(sumWeek * 60.0) %></td>
														<td class="padded">&sum;</td>
													</tr>
													<tr>
<%
														double sumWeekBillable = 0.0;
														dayCounter = 0;
														for(int i = calendarBeginOfWeek.getFirstDayOfWeek(); dayCounter < 7; dayCounter++) {
%>
															<td class="padded_r"><%= wc.decimalMinutesToHhMm(wc.getSumDaysBillable()[i % 7] * 60.0) %></td>
<%
															sumWeekBillable += wc.getSumDaysBillable()[i % 7];
															i++;
														}
%>
														<td class="padded_r"><%= wc.decimalMinutesToHhMm(sumWeekBillable * 60.0) %></td>
														<td class="padded">&sum; (<%= wc.getFieldLabel(CreateWorkAndExpenseRecordController.WORKANDEXPENSERECORD_CLASS, "isBillable", app.getCurrentLocaleAsIndex()) %>)</td>
													</tr>
												</table>
											</td></tr></table>
<%
										}
									}
								}
%>
							</form>
							<script language="javascript" type="text/javascript">
								function setFocus(id) {
									try {
										$(id).focus();
									} catch(e){}
								}
<%
								if (!Boolean.TRUE.equals(wc.getFormFields().getIsInitialized())) {
									%>setFocus('contactXri.Title');	<% 
								} else if (Boolean.TRUE.equals(wc.getFormFields().getIsContactChange())) {
									%>setFocus('contactXri.Title');	<% 
								} else if (Boolean.TRUE.equals(wc.getFormFields().getIsResourceChange())) {
									%>setFocus('resourceXri'); <% 
								} else if (wc.isRecordTypeChange()) {
									%>setFocus('recordType'); <% 
								} else if (wc.getFormFields().getLastFocusId() != null && !wc.getFormFields().getLastFocusId().isEmpty()) {
									%>setFocus('<%= wc.getFormFields().getLastFocusId() %>');<% 
								}
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
<%= mustReload ? "<script language='javascript' type='text/javascript'>$('Reload.button').click();</script>" : "" %>
</body>
</html>
<t:wizardClose controller="<%= wc %>" />
