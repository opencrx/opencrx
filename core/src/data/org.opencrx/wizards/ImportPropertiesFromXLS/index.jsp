<%@page contentType="text/html;charset=UTF-8" language="java" pageEncoding="UTF-8" %>
<%@taglib prefix="t" tagdir="/WEB-INF/tags" %>
<%
/*
 * ====================================================================
 * Project:     opencrx, http://www.opencrx.org/
 * Description: import properties from Excel Sheet
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
	final String FORM_NAME = "ImportPropertiesFromXLS";	
	ImportPropertiesFromXlsController wc = new ImportPropertiesFromXlsController(false);
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
<style type="text/css">
	.err {background-color:orange;color:black;}
	.match {background-color:lightgreen;color:black;}
    .sheetInfo {background-color:gold;}
    .sheetInfo td {padding:3px;}
</style>
<%
if(wc.getErrorMessage() != null && !wc.getErrorMessage().isEmpty()) {
%>
	<br />
	<span style="color:red;" onmouseover="javascript:$('WaitIndicator').style.display='none';"><b><u>ERROR:</u> <%= wc.getErrorMessage() %></b></span>
	<br />
	<br />
<%
} else if("OK".equals(wc.getCommand())) {
%>
	<br />
	<table class="gridTableFull" onmouseover="javascript:$('WaitIndicator').style.display='none';">
		<%= wc.getImportReport() %>
	</table>
	<br />
<%
} else {
%>
	<div id="WaitIndicator" style="width:50px;height:24px;display:none" class="wait">&nbsp;</div>
	<div id="SubmitArea" style="display:block;">
		<form name="UploadMedia" enctype="multipart/form-data" accept-charset="UTF-8" method="post" target="OperationDialogResponse" action="<%= wc.getServletPath() %>">
			<input type="hidden" class="valueL" name="<%= Action.PARAMETER_OBJECTXRI %>" value="<%= wc.getObjectIdentity().toXRI() %>" />
			<input type="hidden" name="<%= Action.PARAMETER_REQUEST_ID %>" value="<%= wc.getRequestId() %>" />
			<input type="hidden" id="Command" name="Command" value="" />	
	        <div class="fieldGroupName">
	          <span style="font-size:9px;">(Hint: row 1 contains field names, data starts at row 2)</span>
	        </div>
        	<br>
	        <table class="fieldGroup">
				<tr id="submitFilename">
					<td class="<%= CssClass.fieldLabel %>"><span class="nw">File:</span></td>
		            <td >
		                <input type="file" class="valueL" size="100" name="<%= ImportPropertiesFromXlsController.UPLOAD_FILE_FIELD_NAME %>" tabindex="500" />
		            </td>
		            <td class="addon" >&nbsp;<br>&nbsp;</td>
				</tr>
				<tr id="submitButtons">
					<td class="<%= CssClass.fieldLabel %>" colspan="3">
						<input type="Submit" name="OK" class="<%= CssClass.btn.toString() %> <%= CssClass.btn_light.toString() %>" tabindex="1000" value="<%= wc.getTexts().getOkTitle() %>" onclick="javascript:$('WaitIndicator').style.display='block';$('SubmitArea').style.display='none';$('Command').value=this.name;" />
						<input type="Submit" name="Cancel" class="<%= CssClass.btn.toString() %> <%= CssClass.btn_light.toString() %>" tabindex="1010" value="<%= app.getTexts().getCancelTitle() %>" onclick="javascript:$('UserDialog').innerHTML='';return false;" />
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
%>
<t:wizardClose controller="<%= wc %>" />
