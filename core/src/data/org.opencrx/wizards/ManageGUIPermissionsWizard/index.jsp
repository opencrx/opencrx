<%@page contentType="text/html;charset=UTF-8" language="java" pageEncoding="UTF-8" %>
<%@taglib prefix="t" tagdir="/WEB-INF/tags" %>
<%
/*
 * ====================================================================
 * Project:     openCRX/Core, http://www.opencrx.org/
 * Description: ManageGUIPermissionsWizard
 * Owner:       CRIXP AG, Switzerland, http://www.crixp.com
 * ====================================================================
 *
 * This software is published under the BSD license
 * as listed below.
 *
 * Copyright (c) 2012-2014, CRIXP Corp., Switzerland
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
	final String FORM_NAME = "ManageGUIPermissionsForm";	
	ManageGUIPermissionsWizardController wc = new ManageGUIPermissionsWizardController();
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
	<table class="tableLayout">
		<tr>
			<td class="cellObject">
				<div class="panel" id="panel<%= FORM_NAME %>" style="display:block;overflow:visible;">
					<table>
						<tr>
							<td style="vertical-align:middle">Role:</td>
							<td style="vertical-align:middle">
								<select name="role" onchange="javascript:$('Refresh.Button').click();return false;">
<%
									org.openmdx.security.realm1.cci2.RoleQuery roleQuery = (org.openmdx.security.realm1.cci2.RoleQuery)pm.newQuery(org.openmdx.security.realm1.jmi1.Role.class);
									roleQuery.orderByName().ascending();
									List<org.openmdx.security.realm1.jmi1.Role> roles = wc.getPolicy().getRole(roleQuery);
									for(org.openmdx.security.realm1.jmi1.Role role: roles) {
%>
										<option value="<%= role.getName() %>" <%= wc.getRoleName().equals(role.getName()) ? "selected" : "" %>><%= role.getName() %></option>
<%
									}
%>
								</select>
							</td>
						</tr>
						<tr>
							<td style="vertical-align:middle">View:</td>
							<td style="vertical-align:middle">
								<input type="radio" name="view" <%= ManageGUIPermissionsWizardController.WizardState.VIEW_OPERATION_PERMISSIONS.equals(wc.getViewName()) ? "checked" : "" %> value="<%= ManageGUIPermissionsWizardController.WizardState.VIEW_OPERATION_PERMISSIONS %>" onchange="javascript:$('Refresh.Button').click();return false;">Operations</input>
								<input type="radio" name="view" <%= ManageGUIPermissionsWizardController.WizardState.VIEW_FIELD_PERMISSIONS.equals(wc.getViewName()) ? "checked" : "" %> value="<%= ManageGUIPermissionsWizardController.WizardState.VIEW_FIELD_PERMISSIONS %>" onchange="javascript:$('Refresh.Button').click();return false;">Fields</input>
								<input type="radio" name="view" <%= ManageGUIPermissionsWizardController.WizardState.VIEW_GRID_PERMISSIONS.equals(wc.getViewName()) ? "checked" : "" %> value="<%= ManageGUIPermissionsWizardController.WizardState.VIEW_GRID_PERMISSIONS %>" onchange="javascript:$('Refresh.Button').click();return false;">Grids</input>
							</td>
						</tr>
					</table>
<%
					ManageGUIPermissionsWizardController.WizardViewState viewState = wc.getWizardState() == null
						? null
						: wc.getWizardState().getViewStates().get(wc.getViewName());
					if(viewState != null) {
%>
						<table>
							<tr>
								<td>		
									<h2 style="font-size:larger;">Generic permissions:</h2>
									<h3>Available:</h3>													
									<select name="genericPermissions" id="genericPermissions" multiple style="font-family:Courier;font-size:11px;height:10em;min-width:150px;">
<%													
										for(String permission: viewState.genericPermissions) {
%>
											<option value="<%= permission %>"><%=  permission.replace("|", " | ") %></option>
<%					
										}
%>									
									</select>
								</td>
								<td style="vertical-align:middle">
									<input type="submit" name="RemoveGenericPermissions" value="&lt;" onclick="javascript:$('Command').value=this.name;" />
									<input type="submit" name="AddGenericPermissions" value="&gt;" onclick="javascript:$('Command').value=this.name;" />
								</td>
								<td>							
									<h2 style="font-size:larger;">&nbsp;</h2>
									<h3>Current:</h3>
									<select name="currentGenericPermissions" id="currentGenericPermissions" multiple style="font-family:Courier;font-size:11px;height:10em;min-width:150px;">
<%													
										for(String permission: viewState.currentGenericPermissions) {
%>
											<option value="<%= permission %>"><%=  permission.replace("|", " | ") %></option>
<%					
										}
%>									
									</select>
								</td>
							</tr>

							<tr>
								<td>
									<h2 style="font-size:larger;">Permissions for <i><%= obj.refClass().refMofId() %></i>:</h2>								
									<h3>Available:</h3>													
									<select name="specificPermissions" id="specificPermissions" multiple style="font-family:Courier;font-size:11px;height:10em;min-width:150px;">
<%													
										for(String permission: viewState.specificPermissions) {
%>
											<option value="<%= permission %>"><%=  permission.replace("|", " | ") %></option>
<%					
										}
%>									
									</select>
								</td>
								<td style="vertical-align:middle">
									<input type="submit" name="RemoveSpecificPermissions" value="&lt;" onclick="javascript:$('Command').value=this.name;" />
									<input type="submit" name="AddSpecificPermissions" value= "&gt;" onclick="javascript:$('Command').value=this.name;" />
								</td>
								<td>
									<h2 style="font-size:larger;">&nbsp;</h2>								
									<h3>Current:</h3>
									<select name="currentSpecificPermissions" id="currentSpecificPermissions" multiple style="font-family:Courier;font-size:11px;height:10em;min-width:150px;">
<%													
										for(String permission: viewState.currentSpecificPermissions) {
%>
											<option value="<%= permission %>"><%=  permission.replace("|", " | ") %></option>
<%					
										}
%>									
									</select>
								</td>
							</tr>
						</table>
<%
					}
%>
				</div>	
				<input type="submit" name="Refresh" id="Refresh.Button" class="<%= CssClass.btn.toString() %> <%= CssClass.btn_light.toString() %>" tabindex="9000" value="<%= app.getTexts().getReloadText() %>" onclick="javascript:$('Command').value=this.name;" />
<%
				if(wc.isCurrentUserIsAdmin() && wc.getWizardState().isDirty()) {
%>				
					<input type="submit" name="Apply" id="Apply.Button" class="<%= CssClass.btn.toString() %> <%= CssClass.btn_light.toString() %>" tabindex="9010" value="Apply" onclick="javascript:$('Command').value=this.name;" />
<%
				}
%>
				<input type="submit" name="Cancel" class="<%= CssClass.btn.toString() %> <%= CssClass.btn_light.toString() %>" tabindex="9020" value="<%= app.getTexts().getCancelTitle() %>" onclick="javascript:$('Command').value=this.name;" />
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
