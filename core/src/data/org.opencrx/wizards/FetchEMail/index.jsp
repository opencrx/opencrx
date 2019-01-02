<%@page contentType="text/html;charset=UTF-8" language="java" pageEncoding="UTF-8" %>
<%@taglib prefix="t" tagdir="/WEB-INF/tags" %>
<%
/*
 * ====================================================================
 * Project:     openCRX/Core, http://www.openmdx.org/
 * Description: FetchEMail
 * Owner:       CRIXP AG, Switzerland, http://www.crixp.com
 * ====================================================================
 *
 * This software is published under the BSD license
 * as listed below.
 *
 * Copyright (c) 2010-2013, CRIXP AG, Switzerland
 * All rights reserved.
 *
 * Redistribution and use in source and binary forms, with or
 * without modification, are permitted provided that the following
 * conditions are met:
 *
 * * Redistributions of source code must retain the above copyright
 * notice, this list of conditions and the following disclaimer.
 *
 * * Redistributions in binary form must reproduce the above copyright
 * notice, this list of conditions and the following disclaimer in
 * the documentation and/or other materials provided with the
 * distribution.
 *
 * * Neither the name of the openMDX team nor the names of its
 * contributors may be used to endorse or promote products derived
 * from this software without specific prior written permission.
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
 * This product includes software developed by Mihai Bazon
 * (http://dynarch.com/mishoo/calendar.epl) published with an LGPL
 * license.
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
	final String FORM_NAME = "FetchEMail";	
	FetchEMailController wc = new FetchEMailController();
%>
	<t:wizardHandleCommand controller='<%= wc %>' defaultCommand='Refresh' assertRequestId='false' />
<%
	if(response.getStatus() != HttpServletResponse.SC_OK) {
		wc.close();
		return;
	}
	ApplicationContext app = wc.getApp();
	javax.jdo.PersistenceManager pm = wc.getPm();
	RefObject_1_0 obj = wc.getObject();
	int tabIndex = 100;	
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
	<input type="hidden" name="<%= Action.PARAMETER_REQUEST_ID %>" value="<%= wc.getRequestId() %>" />
	<input type="hidden" name="<%= Action.PARAMETER_OBJECTXRI %>" value="<%= wc.getObjectIdentity().toXRI() %>" />
	<input type="checkbox" style="display:none;" name="isInitialized" checked />
	<input type="hidden" id="Command" name="Command" value="" />
	<table class="fieldGroup">
		<tr>
			<td class="<%= CssClass.fieldLabel %>"><span class="nw">Host</span></td>
			<td nowrap><input type="text" class="valueL" name="host" tabindex="<%= tabIndex++ %>" value="<%= wc.getFormFields().getHost() %>" /></td>
			<td class="addon" />
		</tr>
		<tr>
			<td class="<%= CssClass.fieldLabel %>"><span class="nw">Protocol:</span></td>
			<td nowrap>
				<select class="valueL" name="protocol" id="protocol" tabindex="<%= tabIndex++ %>" onChange="javascript:
					if(this.value=='imap')  {document.getElementById('port').value='143';}
					if(this.value=='imaps') {document.getElementById('port').value='993';}
					if(this.value=='pop3')  {document.getElementById('port').value='110';}
					if(this.value=='pop3s') {document.getElementById('port').value='995';}
					document.getElementById('reload.button').click();
				  ">
					<option value="imap"  <%= wc.getFormFields().getProtocol() != null && "imap".equals(wc.getFormFields().getProtocol()) ? "selected" : "" %>>imap</option>
					<option value="imaps" <%= wc.getFormFields().getProtocol() != null && "imaps".equals(wc.getFormFields().getProtocol()) ? "selected" : "" %>>imaps</option>
					<option value="pop3"  <%= wc.getFormFields().getProtocol() != null && "pop3".equals(wc.getFormFields().getProtocol()) ? "selected" : "" %>>pop3</option>
					<option value="pop3s" <%= wc.getFormFields().getProtocol() != null && "pop3s".equals(wc.getFormFields().getProtocol()) ? "selected" : "" %>>pop3s</option>
				</select>
			</td>
			<td class="addon" />
		</tr>
		<tr>
			<td class="<%= CssClass.fieldLabel %>"><span class="nw">Port:</span></td>
			<td nowrap><input type="text" class="valueL" name="port" id="port" tabindex="<%= tabIndex++ %>" value="<%= wc.getFormFields().getPort() %>" /></td>
			<td class="addon" />
		</tr>
		<tr>
			<td class="<%= CssClass.fieldLabel %>"><span class="nw">User:</span></td>
			<td nowrap><input type="text" class="valueL" name="user" tabindex="<%= tabIndex++ %>" value="<%= wc.getFormFields().getUser() %>" /></td>
			<td class="addon" />
		</tr>
		<tr>
			<td class="<%= CssClass.fieldLabel %>"><span class="nw">Password:</span></td>
			<td nowrap><input type="password" class="valueL" name="password" tabindex="<%= tabIndex++ %>" value="<%= wc.getFormFields().getPassword() %>" /></td>
			<td class="addon" />
		</tr>
		<tr>
			<td class="<%= CssClass.fieldLabel %>"><span class="nw">Max messages to import:</span></td>
			<td nowrap><input type="text" class="valueL" name="messageCount" tabindex="<%= tabIndex++ %>" value="<%= wc.getFormFields().getMessageCount() %>" /></td>
			<td class="addon" />
		</tr>
           <tr>
             <td class="<%= CssClass.fieldLabel %>"><span class="nw"><%= app.getLabel(FetchEMailController.ACTIVITYCREATOR_CLASS) %>:</span></td>
             <td>
               <select class="valueL" name="activityCreatorXri" tabindex="<%= tabIndex++ %>">
                 <option value="0">Default</option>
<%
					org.opencrx.kernel.activity1.cci2.ActivityCreatorQuery activityCreatorFilter = (org.opencrx.kernel.activity1.cci2.ActivityCreatorQuery)wc.getPm().newQuery(org.opencrx.kernel.activity1.jmi1.ActivityCreator.class);
					activityCreatorFilter.orderByName().ascending();
					activityCreatorFilter.forAllActivityType().activityClass().equalTo(new Short((short)0));
					int maxCreator = 200;
					int counter = 0;
					org.opencrx.kernel.activity1.jmi1.Segment activitySegment = org.opencrx.kernel.backend.Activities.getInstance().getActivitySegment(pm, wc.getProviderName(), wc.getSegmentName());
					for(org.opencrx.kernel.activity1.jmi1.ActivityCreator activityCreator: activitySegment.getActivityCreator(activityCreatorFilter)) {
						String selectedModifier = "";
                        counter++;
                        selectedModifier = (wc.getFormFields().getActivityCreatorXri() != null) && wc.getFormFields().getActivityCreatorXri().equals((activityCreator.refGetPath().toXRI())) ? "selected" : "";
%>
                        <option <%= selectedModifier %> value="<%= activityCreator.refGetPath().toXRI() %>"><%= activityCreator.getName() != null ? activityCreator.getName() : "???" %><%= activityCreator.getDescription() != null ? " / " + activityCreator.getDescription() : "" %></option>
<%
					}
%>
			</select>
    		  </td>
     		  <td class="addon">&nbsp;</td>
     		</tr>
	</table>
	<br />
	<div id="WaitIndicator" style="width:50px;height:24px;" class="wait">&nbsp;</div>
	<div id="SubmitArea" style="display:none;">										    	    				
	    <input type="Submit" name="Refresh" tabindex="1000" value="<%= app.getTexts().getReloadText() %>" onclick="javascript:$('WaitIndicator').style.display='block';$('SubmitArea').style.display='none';$('Command').value=this.name;"/>
	    <input type="Submit" name="OK" tabindex="1010" value="<%= app.getTexts().getOkTitle() %>" onclick="javascript:$('WaitIndicator').style.display='block';$('SubmitArea').style.display='none';$('Command').value=this.name;"/>
	    <input type="Submit" name="Cancel" tabindex="1020" value="<%= app.getTexts().getCancelTitle() %>" onclick="javascript:$('Command').value=this.name;" />
	</div>		
</form>
<br>
<%
if("OK".equals(wc.getCommand())) {
%>
	<table><tr>
<%
		if(wc.getImportedMessages() == null || wc.getImportedMessages().isEmpty()) {
%>
			<td colspan="5">No unread messages found</td>
<%
		} else {
%>
			<td><table class="gridTableFull">
				<tr class="gridTableHeaderFull">
					<td>Sent</td>
					<td>From</td>
					<td>Subject</td>
					<td>Message ID</td>
					<td>Imported Message</td>
				</tr>
<%					
				for(FetchEMailController.ImportedMessage importedMessage: wc.getImportedMessages()) {
%>
					<tr class="gridTableRowFull">
						<td><%= importedMessage.getMimeMessage().getSentDate() %></td>
						<td><%= app.getHtmlEncoder().encode(importedMessage.getMimeMessage().getFrom()[0].toString(), false) %></td>
						<td><%= app.getHtmlEncoder().encode(importedMessage.getMimeMessage().getSubject(), false) %></td>
						<td><%= app.getHtmlEncoder().encode(importedMessage.getMimeMessage().getMessageID() == null ? "NA" : importedMessage.getMimeMessage().getMessageID(), false) %></td>
<%
						if(importedMessage.getEmails() != null) {
%>
							<td class="ok">
<%
								for(org.opencrx.kernel.activity1.jmi1.EMail email: importedMessage.getEmails()) {
									String emailHref = "";
									Action action = new ObjectReference(email, app).getSelectObjectAction();
%>
									<a href="<%= action.getEncodedHRef() %>" target="_blank"><%= "#" + email.getActivityNumber() + " - " + email.getName() %></a>
<%																	
								}
%>
							</td>
<%
						} else {
%>
						<td class="error">failed</td>
<%
					}
%>
					<tr>							
<%
				}
%>
			</table></td>
<%			
		}
%>
	</tr></table>
<%
}
%>
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
