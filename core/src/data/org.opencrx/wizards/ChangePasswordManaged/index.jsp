<%@page contentType="text/html;charset=UTF-8" language="java" pageEncoding="UTF-8" %>
<%@taglib prefix="t" tagdir="/WEB-INF/tags" %>
<%
/*
 * ====================================================================
 * Project:     openCRX/Core, http://www.opencrx.org/
 * Description: ChangePasswordManaged
 * Owner:       CRIXP AG, Switzerland, http://www.crixp.com
 * ====================================================================
 *
 * This software is published under the BSD license
 * as listed below.
 *
 * Copyright (c) 2012-2013, CRIXP Corp., Switzerland
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
<%@ page session="true" import="
java.util.*,
java.io.*,
java.net.*,
java.text.*,
org.openmdx.base.accessor.jmi.cci.*,
org.openmdx.portal.servlet.*,
org.openmdx.portal.servlet.attribute.*,
org.openmdx.portal.servlet.component.*,
org.openmdx.portal.servlet.control.*,
org.openmdx.portal.servlet.wizards.*,
org.openmdx.base.naming.*,
org.openmdx.kernel.log.*,
org.opencrx.portal.wizard.*,
org.openmdx.kernel.id.cci.*,
org.openmdx.kernel.id.*,
org.openmdx.base.exception.*
" %>
<%
	final String FORM_NAME = "ChangePasswordForm";
	ChangePasswordManagedController wc = new ChangePasswordManagedController();
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
<form id="<%= FORM_NAME %>" name="<%= FORM_NAME %>" accept-charset="UTF-8" method="POST" action="<%= wc.getServletPath() %>">
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
	<input type="hidden" name="<%= Action.PARAMETER_OBJECTXRI %>" value="<%= wc.getObjectIdentity().toXRI() %>"/>
	<input type="hidden" name="<%= Action.PARAMETER_REQUEST_ID %>" value="<%= wc.getRequestId() %>" />
	<input type="hidden" id="Command" name="Command" value="" />
	<table class="tableLayout">
		<tr>
			<td class="cellObject">
				<table class="fieldGroup">
					<tr>
						<td class="<%= CssClass.fieldLabel %>">Password <b>old</b>:</td>
						<td><input type="<%= Boolean.TRUE.equals(wc.getShowPasswords()) ? "text" : "password" %>" id="pw_old" name="pw_old"  value="<%= wc.getPwOld() == null ? "" :  wc.getPwOld() %>"/></td>
					</tr>
					<tr>
						<td class="<%= CssClass.fieldLabel %>">Password <b>new</b>:</td>
						<td><input type="<%= Boolean.TRUE.equals(wc.getShowPasswords()) ? "text" : "password" %>" id="pw_new1" name="pw_new1"  value="<%= wc.getPwNew1() == null ? "" :  wc.getPwNew1() %>"/></td>
					</tr>
					<tr>
						<td class="<%= CssClass.fieldLabel %>">Password <b>new</b> (again):&nbsp;</td>
						<td><input type="<%= Boolean.TRUE.equals(wc.getShowPasswords()) ? "text" : "password" %>" id="pw_new2" name="pw_new2"  value="<%= wc.getPwNew2() == null ? "" :  wc.getPwNew2() %>"/></td>
					</tr>
					<tr>
						<td class="<%= CssClass.fieldLabel %>">Show password:</td>
						<td><input type="checkbox" name="showPasswords" id="showPasswords" <%= Boolean.TRUE.equals(wc.getShowPasswords()) ? "checked" : ""	%> onclick="javascript:
							str = 'text';
							if(document.getElementById('pw_old').type==str){str='password';};
							document.getElementById('pw_old').type=str;
							document.getElementById('pw_new1').type=str;
							document.getElementById('pw_new2').type=str;" />
						</td>
					</tr>
				</table>
<%
				if(!wc.getErrorMsg().isEmpty()) {
%>
					<div style="background-color:red;color:white;border:1px solid black;padding:10px;font-weight:bold;margin-top:10px;">
						<%= wc.getErrorMsg() %>
					</div>
<%
				}
				if(wc.getResultText() != null && !wc.getResultText().isEmpty()) {
%>
					<div style="background-color:#CEFFBB;border:1px solid black;padding:10px;font-weight:bold;margin-top:10px;">
						<%= wc.getResultText() %>
					</div>
<%
				}
%>
				<br />
				<div id="WaitIndicator" style="float:left;width:50px;height:24px;" class="wait">&nbsp;</div>
				<div id="SubmitArea" style="float:left;display:none;">
					<input type="submit" name="Refresh" id="Refresh.Button" class="<%= CssClass.btn.toString() %> <%= CssClass.btnDefault.toString() %>" tabindex="9000" value="<%= app.getTexts().getReloadText() %>" onclick="javascript:$('WaitIndicator').style.display='block';$('SubmitArea').style.display='none'; $('Command').value=this.name;" />
					<input type="submit" name="OK" id="OK.Button" class="<%= CssClass.btn.toString() %> <%= CssClass.btnDefault.toString() %>" tabindex="9010" value="<%= app.getTexts().getSaveTitle() %>" onclick="javascript:$('WaitIndicator').style.display='block';$('SubmitArea').style.display='none'; $('Command').value=this.name;this.name='---';" />
					<input type="submit" name="Cancel" class="<%= CssClass.btn.toString() %> <%= CssClass.btnDefault.toString() %>" tabindex="9020" value="<%= app.getTexts().getCancelTitle() %>" onclick="javascript:$('WaitIndicator').style.display='block';$('SubmitArea').style.display='none'; $('Command').value=this.name;" />
				</div>
			</td>
		</tr>
	</table>				
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
</script>
<t:wizardClose controller="<%= wc %>" />
