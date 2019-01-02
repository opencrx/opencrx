<%@page contentType="text/html;charset=UTF-8" language="java" pageEncoding="UTF-8" %>
<%@taglib prefix="t" tagdir="/WEB-INF/tags" %>
<%
/*
 * ====================================================================
 * Project:	    openCRX/Core, http://www.opencrx.org/
 * Description: Set timer on CrxObject
 * Owner:	    CRIXP Corp., Switzerland, http://www.crixp.com
 * ====================================================================
 *
 * This software is published under the BSD license
 * as listed below.
 *
 * Copyright (c) 2013, CRIXP Corp., Switzerland
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
%>
<%@page session="true" import="
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
	final String FORM_NAME = "SetTimerOnCrxObject";	
	SetTimerOnCrxObjectController wc = new SetTimerOnCrxObjectController();
%>
	<t:wizardHandleCommand controller='<%= wc %>' defaultCommand='Refresh' />
<%
	if(response.getStatus() != HttpServletResponse.SC_OK) {
		wc.close();
		return;
	}
	org.openmdx.portal.servlet.ApplicationContext app = wc.getApp();
	javax.jdo.PersistenceManager pm = wc.getPm();	
%>
<div class="OperationDialogTitle"><%= wc.getToolTip() %></div>
<form name="<%= FORM_NAME %>" id="<%= FORM_NAME %>" accept-charset="UTF-8" method="post" action="<%= wc.getServletPath() %>">
<%
	if(wc.getErrorMessage() != null && !wc.getErrorMessage().isEmpty()) {
%>
		<div class="alert alert-danger" role="alert">
		  <table>
		    <tr>
		    	<td style="vertical-align:top;padding:10px;"><span class="glyphicon glyphicon-exclamation-sign"></span></td>
		    	<td><%= wc.getErrorMessage() %></td>
		    </tr>
		  </table>
		</div>
<%
	}
%>
	<input type="hidden" name="<%= Action.PARAMETER_OBJECTXRI %>" value="<%= wc.getObjectIdentity().toXRI() %>" />
	<input type="hidden" name="<%= Action.PARAMETER_REQUEST_ID %>" value="<%= wc.getRequestId() %>" />
	<input type="hidden" name="Command" id="Command" value="" />
<%
	if(wc.hasPermission()) {
%>
		<table class="tableLayout">
			<tr>
				<td class="cellObject">
<%
					if (!wc.isTriggerAtIsInTheFuture()) {
%>
						<div style="background-color:red;color:white;border:1px solid black;padding:10px;font-weight:bold;margin-top:10px;">
							Timer is in the past
						</div>
<%
					}
%>
			  		<table class="fieldGroup">
						<tr>
				  			<td class="<%= CssClass.fieldLabel %>">
								<span class="nw"><%= wc.getFieldLabel(SetTimerOnCrxObjectController.TIMER_CLASS, "name", app.getCurrentLocaleAsIndex()) %>:</span>
							</td>
							<td>
								<input type="text" class="valueL lightUp" name="name" id="name" maxlength="50" tabindex="200" value="<%= wc.getName().replaceAll("\"", "&quot;") %>" />
							</td>
							<td class="addon"></td>
						</tr>
						<tr>
							<td class="<%= CssClass.fieldLabel %>">
								<span class="nw"><%= wc.getFieldLabel(SetTimerOnCrxObjectController.TIMER_CLASS, "timerStartAt", app.getCurrentLocaleAsIndex()) %>:</span>
							</td>
							<td style="padding-top:2px;">
								<input type="text" class="valueL <%= wc.isTriggerAtDateOk() ? "lightUp" : "valueError" %>" name="triggerAt" id="triggerAt" maxlength="16" tabindex="800" value="<%= wc.getTriggerAt() %>" />
							</td>
							<td class="addon">
								<a><img class="popUpButton" id="cal_trigger_triggerAt" border="0" alt="Click to open Calendar" src="./images/cal.gif" /></a>
								<script type="text/javascript">
									  Calendar.setup({
										inputField: "triggerAt",
										ifFormat: "%d-%m-%Y %H:%M",
										timeFormat: "24",
										button: "cal_trigger_triggerAt",
										align: "Tr",
										singleClick: true,
										showsTime: true
									  });
								</script>
								<img class="popUpButton" border="0" title="-15 Min. / aktuelle Zeit" alt="" src="./images/arrow_smallleft.gif" onclick="javascript:$('triggerAt').value = timeTick($('triggerAt').value, -15);$('triggerAt').className='valueL lightUp';" /><img class="popUpButton" border="0"  title="+15 Min. / aktuelle Zeit" alt="" src="./images/arrow_smallright.gif" onclick="javascript:$('triggerAt').value = timeTick($('triggerAt').value, 15);$('triggerAt').className='valueL lightUp';" />
							</td>
						</tr>
					</table>
				</td>
			</tr>
		</table>
<%
	}
%>
	<div id="WaitIndicator" style="float:left;width:50px;height:24px;" class="wait">&nbsp;</div>
	<div id="SubmitArea" style="float:left;display:none;">
<%
	if(!wc.hasPermission()) {
%>		
		<h1><font color="red">No Permission</font></h1>
<%		
	} 
	else {
%>	
		<input type="Submit" name="OK" class="<%= CssClass.btn.toString() %> <%= CssClass.btnDefault.toString() %>" tabindex="9000" value="<%= app.getTexts().getOkTitle() %>" onclick="javascript:$('WaitIndicator').style.display='block';$('SubmitArea').style.display='none'; $('Command').value=this.name;" />
<%
	}
%>		
		<input type="Submit" name="Cancel" class="<%= CssClass.btn.toString() %> <%= CssClass.btnDefault.toString() %>" tabindex="9010" value="<%= app.getTexts().getCancelTitle() %>" onClick="javascript:$('WaitIndicator').style.display='block';$('SubmitArea').style.display='none'; $('Command').value=this.name;" />		
	</div>
	<br />
</form>
<br />
<script type="text/javascript">
	Event.observe('<%= FORM_NAME %>', 'submit', function(event) {
		$('<%= FORM_NAME %>').request({
			onFailure: function() { },
			onSuccess: function(t) {
				$('UserDialog').update(t.responseText);
			}
		});
		Event.stop(event);
	});
	$('WaitIndicator').style.display='none';
	$('SubmitArea').style.display='block';
	
	timeTick = function (dd_mm_yyyy_hh_dd, upMins) {
		var right_now = new Date();
  		var dateTime = dd_mm_yyyy_hh_dd.split(" ");
  		var hrs = right_now.getHours();
  		var mins = right_now.getMinutes();
  		try {
			dateStr = dateTime[0];
			timeStr = dateTime[1].split(":");
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
  		while (mins < 0) {mins += 24*60;}
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
  		if (dateStr.length < 10) {
			dateStr = ((right_now.getDate() < 10) ? "0" : "") + right_now.getDate() + "-" + ((right_now.getMonth() < 9) ? "0" : "") + (right_now.getMonth()+1) + "-" + right_now.getFullYear();
  		}
		return dateStr + " " + hrsStr + ":" + minsStr;
	}
	
</script>
<t:wizardClose controller="<%= wc %>" />

