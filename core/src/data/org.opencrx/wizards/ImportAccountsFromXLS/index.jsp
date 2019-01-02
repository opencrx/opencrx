<%@page contentType="text/html;charset=UTF-8" language="java" pageEncoding="UTF-8" %>
<%@taglib prefix="t" tagdir="/WEB-INF/tags" %>
<%
/*
 * ====================================================================
 * Project:     opencrx, http://www.opencrx.org/
 * Description: import accounts from Excel Sheet (xls or xslx format)
 * Owner:       CRIXP Corp., Switzerland, http://www.crixp.com
 * ====================================================================
 *
 * This software is published under the BSD license
 * as listed below.
 *
 * Copyright (c) 2008-2013, CRIXP Corp., Switzerland
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
	final String FORM_NAME = "ImportAccountsFromXLS";	
	ImportAccountsFromXlsController wc = new ImportAccountsFromXlsController(false);
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
	boolean isUpdateProgressMeter = "UpdateProgressMeter".equals(wc.getCommand());
%>
<%
if(!isUpdateProgressMeter) {
	session.setAttribute(wc.getWizardName(), null);
%>
	<br />
	<div class="OperationDialogTitle"><%= wc.getToolTip() %></div>
	<style type="text/css">
		.ok {background-color:#EEFF86;color:black;}
		.err {background-color:orange;color:black;}
		.match {background-color:lightgreen;color:black;}
	    .sheetInfo {background-color:gold;}
	    .sheetInfo td {padding:3px;}
	</style>
<%
}
if(wc.getErrorMessage() != null && !wc.getErrorMessage().isEmpty()) {
%>
	<br />
	<span style="color:red;"><b><u>ERROR:</u> <%= wc.getErrorMessage() %></b></span>
	<br />
	<br />
<%
} else if("OK".equals(wc.getCommand())) {
%>
	<br />
	<table class="gridTableFull" onmouseover="javascript:$('WaitIndicator').style.display='none';">
<%
		ImportAccountsFromXlsController.ProgressMeter progressMeter = new ImportAccountsFromXlsController.ProgressMeter();
		session.setAttribute(wc.getWizardName() + wc.getRequestId(), progressMeter);
		wc.performImport(out, progressMeter); 
%>
	</table>
	<br />
<%
} else {
%>
	<div id="WaitIndicator" style="display:<%= isUpdateProgressMeter ? "block" : "none" %>" class="wait">
		<br />
		<br />
<%
		ImportAccountsFromXlsController.ProgressMeter progressMeter = (ImportAccountsFromXlsController.ProgressMeter)session.getAttribute(wc.getWizardName() + wc.getRequestId());
		if(progressMeter != null) {
%>
			<%= progressMeter.getCurrent() %> / <%= progressMeter.getTotal() %>
<%		
		}
%>			
		<form name="ProgressMeterForm" id="ProgressMeterForm" accept-charset="UTF-8" method="POST" action="<%= "../.." + request.getServletPath() %>">
			<input type="hidden" class="valueL" name="<%= Action.PARAMETER_OBJECTXRI %>" value="<%= wc.getObjectIdentity().toXRI() %>" />
			<input type="hidden" name="<%= Action.PARAMETER_REQUEST_ID %>" value="<%= wc.getRequestId() %>" />
			<input type="hidden" name="Command" value="UpdateProgressMeter" />
			<input type="Submit" name="Refresh" id="ProgressMeterRefreshButton" class="<%= CssClass.btn.toString() %> <%= CssClass.btnDefault.toString() %>" style="display:none;" value="<%= wc.getTexts().getReloadText() %>" />
		</form>
		<script type="text/javascript">
			Event.observe('ProgressMeterForm', 'submit', function(event) {
				$('ProgressMeterForm').request({
					onFailure: function() { },
					onSuccess: function(t) {
						$('WaitIndicator').update(t.responseText);
					}
				});
				Event.stop(event);
			});
<%
			if(isUpdateProgressMeter) {
%>
				setTimeout('javascript:$(\'ProgressMeterRefreshButton\').click();', 5000);
<%
			}
%>
		</script>
	</div>
<%
	if(!isUpdateProgressMeter)  {
%>	
		<div id="SubmitArea" style="display:block;">				    	    				
			<form name="<%= FORM_NAME %>" enctype="multipart/form-data" accept-charset="UTF-8" method="POST" target="UserDialogResponse" action="<%= wc.getServletPath() %>">
				<input type="hidden" class="valueL" name="<%= Action.PARAMETER_OBJECTXRI %>" value="<%= wc.getObjectIdentity().toXRI() %>" />
				<input type="hidden" name="<%= Action.PARAMETER_REQUEST_ID %>" value="<%= wc.getRequestId() %>" />
				<input type="hidden" id="Command" name="Command" value="" />	
		    	<div class="fieldGroupName">
		      		<span style="font-size:9px;">(Hint: row 1 contains field names, data starts at row 2)</span>
		    	</div>
		    	<br>
		    	<table class="fieldGroup">
		      		<tr id="localeSelector">
		        		<td class="<%= CssClass.fieldLabel %>"><span class="nw">Language of field names:</span></td>
		        		<td >
							<select class="valueL" id="locale" name="locale" tabindex="400">
								<option <%= wc.getLocale() == 0 ? "selected" : "" %> value="0">en (English [FirstName, LastName, ...]</option>
								<option <%= wc.getLocale() == 1 ? "selected" : "" %> value="1">de (Deutsch) [Vorname, Nachname, ...]</option>
							</select>
						</td>
						<td class="addon" >&nbsp;<br>&nbsp;</td>
					</tr>
					<tr id="submitFilename">
						<td class="<%= CssClass.fieldLabel %>"><span class="nw">File:</span></td>
						<td>
							<input type="file" class="valueL" size="100" name="<%= ImportAccountsFromXlsController.UPLOAD_FILE_FIELD_NAME %>" tabindex="500" />
						</td>
						<td class="addon" >&nbsp;<br>&nbsp;</td>
					</tr>
					<tr id="submitButtons">
						<td class="<%= CssClass.fieldLabel %>" colspan="3">
							<input type="Submit" name="OK" tabindex="1000" value="<%= wc.getTexts().getOkTitle() %>" onclick="javascript:$('WaitIndicator').style.display='block';$('SubmitArea').style.display='none';setTimeout('javascript:$(\'ProgressMeterRefreshButton\').click();', 5000);$('Command').value=this.name;" />
							<input type="Submit" name="Cancel" tabindex="1010" value="<%= app.getTexts().getCancelTitle() %>" onclick="javascript:$('UserDialog').innerHTML='';return false;" />
						</td>
						<td></td>
						<td class="addon" >&nbsp;<br>&nbsp;</td>
					</tr>
				</table>
			</form>
		</div>
		<br />
<%
	}
}
%>
<t:wizardClose controller="<%= wc %>" />
