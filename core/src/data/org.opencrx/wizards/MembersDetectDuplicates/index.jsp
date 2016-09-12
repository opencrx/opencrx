<%@page contentType= "text/html;charset=utf-8" language="java" pageEncoding= "UTF-8" %>
<%@taglib prefix="t" tagdir="/WEB-INF/tags" %>
<%
/*
 * ====================================================================
 * Project:     openCRX/Core, http://www.opencrx.org/
 * Description: Detect duplicate members
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
org.opencrx.kernel.backend.*,
org.opencrx.kernel.portal.wizard.*,
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
	final String FORM_NAME = "MembersDetectDuplicates";	
	MembersDetectDuplicatesController wc = new MembersDetectDuplicatesController();
%>
	<t:wizardHandleCommand controller='<%= wc %>' defaultCommand='Refresh' />
<%
	if(response.getStatus() != HttpServletResponse.SC_OK) {
		wc.close();
		return;
	}
	org.openmdx.portal.servlet.ApplicationContext app = wc.getApp();
	javax.jdo.PersistenceManager pm = wc.getPm();	
	TimeZone timezone = TimeZone.getTimeZone(app.getCurrentTimeZone());
	SimpleDateFormat timeFormat = new SimpleDateFormat("dd-MMM-yyyy HH:mm", app.getCurrentLocale());
	timeFormat.setTimeZone(timezone);	
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
	<input type="hidden" name="<%= Action.PARAMETER_OBJECTXRI %>" value="<%= wc.getObjectIdentity().toXRI() %>" />
	<input type="hidden" name="<%= Action.PARAMETER_REQUEST_ID %>" value="<%= wc.getRequestId() %>" />
	<input type="hidden" id="Command" name="Command" value="" />
	<br />
<%
	if("OK".equals(wc.getCommand())) {
%>	
	    <table><tr><td>
			<table id="resultTable" class="gridTableFull">
				<tr class="gridTableHeaderFull">
			        <td class="gridColTypeNormal"><div class="textfilter"><%= wc.getApp().getLabel(MembersDetectDuplicatesController.ACCOUNT_CLASS) %> / <%= app.getLabel(MembersDetectDuplicatesController.MEMBER_CLASS) %></div></td>
			        <td class="gridColTypeNormal" align="center"><div class="textfilter"><%= app.getLabel(MembersDetectDuplicatesController.MEMBER_CLASS) %> <%= wc.getFieldLabel(MembersDetectDuplicatesController.MEMBER_CLASS, "disabled", app.getCurrentLocaleAsIndex()) %></div></td>
			        <td class="gridColTypeNormal" align="center"><div class="textfilter"><%= app.getLabel(MembersDetectDuplicatesController.ACCOUNT_CLASS) %> <%= wc.getFieldLabel(MembersDetectDuplicatesController.ACCOUNT_CLASS, "disabled", app.getCurrentLocaleAsIndex()) %></div></td>
			        <td class="gridColTypeNormal" align="left"><div class="textfilter"><%= app.getLabel(MembersDetectDuplicatesController.MEMBER_CLASS) %> <%= wc.getFieldLabel(MembersDetectDuplicatesController.MEMBER_CLASS, "createdAt", app.getCurrentLocaleAsIndex()) %></div></td>
			        <td class="gridColTypeNormal" align="left"><div class="textfilter"><%= app.getLabel(MembersDetectDuplicatesController.MEMBER_CLASS) %> <%= wc.getFieldLabel(MembersDetectDuplicatesController.MEMBER_CLASS, "createdBy", app.getCurrentLocaleAsIndex()) %></div></td>
			        <td class="gridColTypeNormal" align="center"></td>
				</tr>
<%
				for(org.opencrx.kernel.account1.jmi1.Member member: wc.getMembersToDelete()) {
					try {
		                org.opencrx.kernel.account1.jmi1.Account account = null;
		                String customerInfo = "---";
		                String accountHref = "";
		                if(member.getAccount() != null) {
							customerInfo = (new ObjectReference(member.getAccount(), app)).getTitle();
							account = member.getAccount();
							Action action = new ObjectReference(
								account,
								app
							).getSelectObjectAction();
							accountHref = action.getEncodedHRef();
		                } else {
							continue;
		                }
%>
						<tr class="gridTableRowFull"><!-- 6 columns -->
							<td colspan="6"><b><a href="<%= accountHref %>" target="_blank"><%= customerInfo %></a></b></td>
						</tr>
<%
						org.opencrx.kernel.account1.cci2.MemberQuery memberQuery = (org.opencrx.kernel.account1.cci2.MemberQuery)pm.newQuery(org.opencrx.kernel.account1.jmi1.Member.class);
						memberQuery.thereExistsAccount().equalTo(account);
	                	for(org.opencrx.kernel.account1.jmi1.Member currentMember: ((org.opencrx.kernel.account1.jmi1.Account)wc.getObject()).getMember(memberQuery)) {
		                    String memberHref = "";
		                    Action action = new ObjectReference(
		                        currentMember,
		                        app
		                    ).getSelectObjectAction();
		                    memberHref = action.getEncodedHRef();
%>
							<tr class="gridTableRowFull"><!-- 6 columns -->
								<td><a href="<%= memberHref %>" target="_blank"><%= MembersDetectDuplicatesController.INDENT + (new ObjectReference(currentMember, app)).getTitle() %></a></td>
								<td align="center"><img src="./images/<%= currentMember.isDisabled() != null && currentMember.isDisabled().booleanValue() ? "" : "not" %>checked_r.gif" alt="" /></td>
								<td align="center"><img src="./images/<%= currentMember.getAccount() != null && currentMember.getAccount().isDisabled() != null && currentMember.getAccount().isDisabled().booleanValue() ? "" : "not" %>checked_r.gif" alt="" /></td>
								<td align="left"><%= timeFormat.format(currentMember.getCreatedAt()) %></td>
								<td align="left"><%= currentMember.getCreatedBy() %></td>
								<td></td>
							</tr>
<%
						}
					} catch (Exception ignore) {}
				}
%>
			</table>
		</td></tr></table>
<%
	}
%>		
	<div id="WaitIndicator" style="float:left;width:50px;height:24px;" class="wait">&nbsp;</div>
	<div id="SubmitArea" style="float:left;display:none;">				
		<input type="submit" name="OK" class="<%= CssClass.btn.toString() %> <%= CssClass.btnDefault.toString() %>" tabindex="9010" value="<%= app.getTexts().getOkTitle() %>" onClick="javascript:$('WaitIndicator').style.display='block';$('SubmitArea').style.display='none'; $('Command').value=this.name;" />
		<input type="submit" name="Cancel" class="<%= CssClass.btn.toString() %> <%= CssClass.btnDefault.toString() %>" tabindex="9020" value="<%= app.getTexts().getCancelTitle() %>" onclick="javascript:$('WaitIndicator').style.display='block';$('SubmitArea').style.display='none'; $('Command').value=this.name;" />
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
</script>
<t:wizardClose controller="<%= wc %>" />
