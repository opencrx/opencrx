<%@page contentType="text/html;charset=UTF-8" language="java" pageEncoding="UTF-8" %>
<%@taglib prefix="t" tagdir="/WEB-INF/tags" %>
<%
/*
 * ====================================================================
 * Project:     openCRX/Core, http://www.opencrx.org/
 * Description: ManageChangeRequestsWizard
 * Owner:       CRIXP AG, Switzerland, http://www.crixp.com
 * ====================================================================
 *
 * This software is published under the BSD license
 * as listed below.
 *
 * Copyright (c) 2013-2014, CRIXP Corp., Switzerland
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
	final String FORM_NAME = "ManageChangeRequestsWizardForm";
	ManageChangeRequestsWizardController wc = new ManageChangeRequestsWizardController();
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
	<input type="hidden" name="<%= ManageChangeRequestsWizardController.PARAMETER_CURRENT_CHANGE_REQUEST_XRI %>" id="<%=ManageChangeRequestsWizardController.PARAMETER_CURRENT_CHANGE_REQUEST_XRI %>" value="<%= wc.getSelectedChangeRequest() == null ? "" : wc.getSelectedChangeRequest().refGetPath().toXRI() %>" />
	<input type="hidden" name="<%= ManageChangeRequestsWizardController.PARAMETER_SELECTED_CHANGE_REQUEST_XRI %>" id="<%=ManageChangeRequestsWizardController.PARAMETER_SELECTED_CHANGE_REQUEST_XRI %>" value="" />
	<input type="hidden" name="<%= ManageChangeRequestsWizardController.PARAMETER_PROCESS_TRANSITION_XRI %>" id="<%=ManageChangeRequestsWizardController.PARAMETER_PROCESS_TRANSITION_XRI %>" value="" />
	<input type="hidden" id="Command" name="Command" value="" />
	<table class="tableLayout">
		<tr>
			<td class="cellObject">
				<div class="panel" style="display:block;overflow:visible;">
					<div>&nbsp;</div>
					<table class="gridTableFull">
<%
						int count = 0;
						for(org.opencrx.kernel.activity1.jmi1.Activity changeRequest: wc.getChangeRequests()) {
							boolean isSelected = wc.getSelectedChangeRequest() != null && wc.getSelectedChangeRequest().equals(changeRequest);
%>
							<tr class="gridTableRowFull <%= count % 2 == 0 ? "" : "gridTableFullhover"%>" style="cursor:pointer;background-color:<%=isSelected ? wc.getColor(ManageChangeRequestsWizardController.Format.CHANGE_REQUEST) : "none"%>;" onclick="javascript:if(!$('Command').value){$('<%=ManageChangeRequestsWizardController.PARAMETER_SELECTED_CHANGE_REQUEST_XRI %>').value='<%= changeRequest.refGetPath().toXRI() %>';$('Refresh').click();};">							
<%
								wc.renderChangeRequest(changeRequest, ManageChangeRequestsWizardController.Format.SUMMARY, out);
%>
							</tr>
<%
							count++;
						}
%>
					</table>
<%
					if(wc.getSelectedChangeRequest() != null) {
%>					
						<div>&nbsp;</div>
						<table>
							<tr>
								<td style="width:30%;border-radius:10px;background-color:<%= wc.getColor(ManageChangeRequestsWizardController.Format.CHANGE_REQUEST)%>;">
									<br />
									<%
										wc.renderChangeRequest(wc.getSelectedChangeRequest(), ManageChangeRequestsWizardController.Format.CHANGE_REQUEST, out);
									%>
								</td>
								<td style="width:30%;border-radius:10px;background-color:<%= wc.getColor(ManageChangeRequestsWizardController.Format.EXISTING_OBJECT)%>;">
									<br />
									<%
										wc.renderChangeRequest(wc.getSelectedChangeRequest(), ManageChangeRequestsWizardController.Format.EXISTING_OBJECT, out);
									%>
								</td>
								<td style="width:30%;border-radius:10px;background-color:<%= wc.getColor(ManageChangeRequestsWizardController.Format.MERGED_OBJECT)%>;">
									<br />
									<%
										wc.renderChangeRequest(wc.getSelectedChangeRequest(), ManageChangeRequestsWizardController.Format.MERGED_OBJECT, out);
									%>
									<div id="WaitIndicator2" style="display:none;width:50px;height:24px;" class="wait">&nbsp;</div>
									<div id="SubmitArea2" style="display:none;">
										<input type="submit" name="Apply" tabindex="9010" value="<%= wc.getTexts().getSaveTitle() %>" onclick="javascript:$('WaitIndicator2').style.display='block';$('SubmitArea2').style.display='none';$('Command').value=this.name;" />
									</div>
								</td>
							</tr>
						</table>
						<script type="text/javascript">
							<%= wc.getPostRenderChangeRequestScript().toString() %>
						</script>						
<%
					}
%>
				</div>
				<input type="submit" name="Refresh" id="Refresh" class="<%= CssClass.btn.toString() %> <%= CssClass.btn_light.toString() %>" tabindex="9030" value="<%= app.getTexts().getReloadText() %>" onclick="javascript:$('Command').value=this.name;" />
				<input type="submit" name="Cancel" class="<%= CssClass.btn.toString() %> <%= CssClass.btn_light.toString() %>" tabindex="9040" value="<%= app.getTexts().getCancelTitle() %>" onclick="javascript:$('Command').value=this.name;" />
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
</script>
<t:wizardClose controller="<%= wc %>" />
