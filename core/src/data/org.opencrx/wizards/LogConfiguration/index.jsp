<%@page contentType="text/html;charset=UTF-8" language="java" pageEncoding="UTF-8" %>
<%@taglib prefix="t" tagdir="/WEB-INF/tags" %>
<%
/*
 * ====================================================================
 * Project:     openCRX/Core, http://www.opencrx.org/
 * Description: LogConfiguration wizard
 * Owner:       CRIXP Corp., Switzerland, http://www.crixp.com
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
	final String FORM_NAME = "LogConfiguration";	
	LogConfigurationController wc = new LogConfigurationController();
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
%>
<div class="OperationDialogTitle"><%= wc.getToolTip() %></div>
<form name="<%= FORM_NAME %>" id="<%= FORM_NAME %>" accept-charset="UTF-8" method="POST" action="<%= wc.getServletPath() %>">
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
	<input type="hidden" name="<%= Action.PARAMETER_REQUEST_ID %>" value="<%= wc.getRequestId() %>" />
	<input type="hidden" name="<%= Action.PARAMETER_OBJECTXRI %>" value="<%= wc.getObjectIdentity().toXRI() %>" />
	<input type="hidden" name="Command" id="Command" value="" />
	<div id="SubmitArea">										    	    
		<input type="submit" name="OK" id="OK.Button" class="<%= CssClass.btn.toString() %> <%= CssClass.btnDefault.toString() %>" tabindex="1000" value="<%= wc.getTexts().getOkTitle() %>" onclick="javascript:$('Command').value=this.name;" />
		<input type="submit" name="Cancel" class="<%= CssClass.btn.toString() %> <%= CssClass.btnDefault.toString() %>" tabindex="1010" value="<%= app.getTexts().getCancelTitle() %>" onclick="javascript:$('UserDialog').innerHTML='';return false;" />
	</div>
	<div class="fieldGroupName">&nbsp;</div>
	<table class="fieldGroup">
		<tr>
			<td style="padding:5px 0px;background-color:#FFFED2;vertical-align:middle;"><strong>ALL LOGGERS</strong></td>
			<td width="120px;" style="padding:5px 0px;background-color:#FFFED2;vertical-align:middle;">
				<select name="levelAllLoggers" onchange="javascript:$('OK.Button').click();">
					<option <%= wc.getLevelAllLoggers() == LogConfigurationController.LOG_LEVEL_INDIVIDUAL ? "SELECTED" : "" %> value="<%= LogConfigurationController.LOG_LEVEL_INDIVIDUAL %>">INDIVIDUAL SETTINGS</option>
					<option <%= wc.getLevelAllLoggers() == java.util.logging.Level.SEVERE.intValue() ? "SELECTED" : "" %> value="<%= java.util.logging.Level.SEVERE.intValue()  %>">SEVERE</option>
					<option <%= wc.getLevelAllLoggers() == java.util.logging.Level.WARNING.intValue() ? "SELECTED" : "" %> value="<%= java.util.logging.Level.WARNING.intValue() %>">WARNING</option>
					<option <%= wc.getLevelAllLoggers() == java.util.logging.Level.INFO.intValue() ? "SELECTED" : "" %> value="<%= java.util.logging.Level.INFO.intValue() %>">INFO</option>
					<option <%= wc.getLevelAllLoggers() == java.util.logging.Level.FINE.intValue() ? "SELECTED" : "" %> value="<%= java.util.logging.Level.FINE.intValue() %>">FINE</option>
					<option <%= wc.getLevelAllLoggers() == java.util.logging.Level.FINER.intValue() ? "SELECTED" : "" %> value="<%= java.util.logging.Level.FINER.intValue() %>">FINER</option>
					<option <%= wc.getLevelAllLoggers() == java.util.logging.Level.FINEST.intValue() ? "SELECTED" : "" %> value="<%= java.util.logging.Level.FINEST.intValue() %>">FINEST</option>
				</select>
			</td>
        </tr>
<%
		java.util.logging.LogManager logManager = java.util.logging.LogManager.getLogManager();
		Enumeration<String> loggerNames = logManager.getLoggerNames();
		Set<String> sortedLoggerNames = new TreeSet<String>();
		while(loggerNames.hasMoreElements()) {
			sortedLoggerNames.add(loggerNames.nextElement());
		}
		int ii = 0;
		for(String loggerName: sortedLoggerNames) {
			java.util.logging.Logger logger = logManager.getLogger(loggerName);
			if(logger != null) {
%>
				<tr>
					<td <%= ii % 2 == 0 ? "style='background-color:#E6FEE6;'" : "" %> style="vertical-align:middle;"><%= loggerName.length() == 0 ? "*" : loggerName %></td>
					<td <%= ii % 2 == 0 ? "style='background-color:#E6FEE6;'" : "" %> width="120px;">
						<select name="Logger.<%= loggerName %>" id="Logger.<%= loggerName %>" onchange="javascript:$('OK.Button').click();">
							<option <%= logger.getLevel() == java.util.logging.Level.SEVERE ? "SELECTED" : "" %> value="<%= java.util.logging.Level.SEVERE.intValue() %>">SEVERE</option>
							<option <%= logger.getLevel() == java.util.logging.Level.WARNING ? "SELECTED" : "" %> value="<%= java.util.logging.Level.WARNING.intValue() %>">WARNING</option>
							<option <%= logger.getLevel() == java.util.logging.Level.INFO ? "SELECTED" : "" %>>INFO</option>
							<option <%= logger.getLevel() == java.util.logging.Level.FINE ? "SELECTED" : "" %>>FINE</option>
							<option <%= logger.getLevel() == java.util.logging.Level.FINER ? "SELECTED" : "" %>>FINER</option>
							<option <%= logger.getLevel() == java.util.logging.Level.FINEST ? "SELECTED" : "" %>>FINEST</option>
						</select>
					</td>
		        </tr>
<%
				ii++;
			}
		}
%>
	</table>
	<div class="fieldGroupName">&nbsp;</div>
	<div id="SubmitArea2">
		<input type="Submit" name="OK" id="OK.Button" class="<%= CssClass.btn.toString() %> <%= CssClass.btnDefault.toString() %>" tabindex="1000" value="<%= wc.getTexts().getOkTitle() %>" onclick="javascript:$('Command').value=this.name;" />
		<input type="Submit" name="Cancel" class="<%= CssClass.btn.toString() %> <%= CssClass.btnDefault.toString() %>" tabindex="1010" value="<%= app.getTexts().getCancelTitle() %>" onclick="javascript:$('UserDialog').innerHTML='';return false;" />
	</div>
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
</script>
<t:wizardClose controller="<%= wc %>" />
