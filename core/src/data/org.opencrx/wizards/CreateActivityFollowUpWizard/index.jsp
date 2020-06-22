<%@page contentType="text/html;charset=UTF-8" language="java" pageEncoding="UTF-8" %>
<%@taglib prefix="t" tagdir="/WEB-INF/tags" %>
<%
/*
 * ====================================================================
 * Project:     openCRX/Core, http://www.opencrx.org/
 * Description: CreateActivityFollowUpWizard
 * Owner:       CRIXP AG, Switzerland, http://www.crixp.com
 * ====================================================================
 *
 * This software is published under the BSD license
 * as listed below.
 *
 * Copyright (c) 2004-2014, CRIXP Corp., Switzerland
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
org.opencrx.kernel.portal.*,
org.opencrx.portal.wizard.*,
org.openmdx.kernel.id.cci.*,
org.openmdx.kernel.id.*,
org.openmdx.base.accessor.jmi.cci.*,
org.openmdx.base.exception.*,
org.openmdx.portal.servlet.*,
org.openmdx.portal.servlet.attribute.*,
org.openmdx.portal.servlet.component.*,
org.openmdx.portal.servlet.control.*,
org.openmdx.portal.servlet.wizards.*,
org.openmdx.base.naming.*
"%>
<%
	final String FORM_NAME = "doFollowUpForm";
	CreateActivityFollowUpWizardController wc = new CreateActivityFollowUpWizardController();
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
	ViewPort viewPort = wc.getViewPort(out);
%>
<div class="OperationDialogTitle"><%= wc.getToolTip() %></div>
<form id="<%= FORM_NAME %>" name="<%= FORM_NAME %>" accept-charset="UTF-8" method="POST" action="<%= wc.getServletPath() %>">
<%
	if(wc.getErrorMessage() != null && !wc.getErrorMessage().isEmpty()) {
%>
		<div class="alert alert-danger" role="alert">
		  <table>
		    <tr>
		    	<td><%= wc.getErrorMessage() %></td>
		    </tr>
		  </table>
		</div>
<%
	}
%>
	<input type="hidden" name="<%= Action.PARAMETER_REQUEST_ID %>" value="<%= wc.getRequestId() %>" />
	<input type="hidden" name="<%= Action.PARAMETER_OBJECTXRI %>" value="<%= wc.getObjectIdentity().toXRI() %>" />
	<input type="hidden" id="Command" name="Command" value="" />
	<input type="checkbox" style="display:none;" id="isInitialized" name="isInitialized" checked />
<%
	if(wc.getNextTransitions() == null || !wc.getNextTransitions().isEmpty()) {
%>
      		<table class="tableLayout">
      		<tr>
      			<td class="cellObject">
      				<div class="panel" id="panel<%= FORM_NAME %>" style="display:block;overflow:visible;">
<%
						if(wc.getErrorMessage() != null) {
%>
							<table class="tableError">
							  <tbody><tr>
							    <td class="cellErrorLeft">Error</td>
							    <td class="cellErrorRight"><%= new Date() %></td>
							  </tr>
							  <tr>
							    <td class="cellErrorLeft"></td>
							    <td class="cellErrorRight"><%= wc.getErrorMessage() %></td>
							  </tr>
							</tbody></table>
<%							
						}
						wc.getForms().get(FORM_NAME).paint(viewPort, null, true);
      					viewPort.flush();
%>
						<div class="fieldGroupName">&nbsp;</div>					
						<table class="fieldGroup">
							<tr>
								<td class="<%= CssClass.fieldLabel %>">
									<span class="nw"><%= app.getLabel(CreateActivityFollowUpWizardController.RESOURCE_CLASS) %>:</span>
								</td>
								<td>
									<input type="checkbox" id="reassignActivity" style="display:none;" name="reassignActivity" />
									<select id="assignToXri" name="assignToXri" class="valueL" onchange="javascript:$('reassignActivity').checked='true';$('Refresh.Button').click();" >
<%
										// get Resources sorted by name(asc)
										org.opencrx.kernel.activity1.jmi1.Segment activitySegment = (org.opencrx.kernel.activity1.jmi1.Segment)pm.getObjectById(
											new Path("xri://@openmdx*org.opencrx.kernel.activity1").getDescendant("provider", wc.getProviderName(), "segment", wc.getSegmentName())
										);
						                org.opencrx.kernel.activity1.cci2.ResourceQuery recourceFilter = (org.opencrx.kernel.activity1.cci2.ResourceQuery)pm.newQuery(org.opencrx.kernel.activity1.jmi1.Resource.class);
						                recourceFilter.orderByName().ascending();
										recourceFilter.forAllDisabled().isFalse();
										int maxResourceToShow = 0;
						                for(org.opencrx.kernel.activity1.jmi1.Resource resource: activitySegment.getResource(recourceFilter)) {
						                	try {
												// get resource
												org.opencrx.kernel.account1.jmi1.Contact contact = resource.getContact();
												if (contact != null) {
													String selectedModifier = ((contact != null ) && (wc.getAssignedTo() != null) && (wc.getAssignedTo().refGetPath().equals(contact.refGetPath()))) ? "selected" : "";
%>
													<option <%= selectedModifier %> value="<%= contact.refGetPath().toXRI() %>"><%= resource.getName() + (contact != null ? " (" + contact.getFirstName() + " " + contact.getLastName() + ")": "") %></option>
<%
												}
											} catch (Exception e) {}
						                	maxResourceToShow++;
						                	if(maxResourceToShow > 200) break;
										}
%>
									</select>
								</td>
								<td class="addon"/>
							</tr>
						</table>
      				</div>
					<div id="WaitIndicator" style="float:left;width:50px;height:24px;" class="wait">&nbsp;</div>
					<div id="SubmitArea" style="float:left;display:none;">      				
	      				<input type="submit" name="Refresh" id="Refresh.Button" class="<%= CssClass.btn.toString() %> <%= CssClass.btn_light.toString() %>" tabindex="9000" value="<%= app.getTexts().getReloadText() %>" onclick="javascript:$('Command').value=this.name;" />
	      				<input type="submit" name="OK" id="OK.Button" class="<%= CssClass.btn.toString() %> <%= CssClass.btn_light.toString() %>" tabindex="9000" value="<%= app.getTexts().getSaveTitle() %>" onclick="javascript:$('Command').value=this.name;this.name='--';" />
	      				<input type="submit" name="Cancel" class="<%= CssClass.btn.toString() %> <%= CssClass.btn_light.toString() %>" tabindex="9010" value="<%= app.getTexts().getCancelTitle() %>" onclick="javascript:$('Command').value=this.name;" />
	      			</div>
      			</td>
      		</tr>
      	</table>
<%
	} else {
%>
		<div title="Activity FollowUp Error"  style="background-color:red;color:white;border:1px solid black;padding:10px;font-weight:bold;margin-top:10px;">
			Activity Follow Up Error (maybe the end of the process has been reached?)
		</div>
		<br>
		<input type="submit" name="Cancel" tabindex="9010" value="<%= app.getTexts().getOkTitle() %>" onclick="javascript:$('Command').value=this.name;" />
<%
	}
%>
</form>
<br />
<script type="text/javascript">
	Event.observe('<%= FORM_NAME %>', 'submit', function(event) {
		$('WaitIndicator').style.display='block';
		$('SubmitArea').style.display='none';
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
