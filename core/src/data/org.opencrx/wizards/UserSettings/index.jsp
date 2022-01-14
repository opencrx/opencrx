<%@page contentType="text/html;charset=UTF-8" language="java" pageEncoding="UTF-8" %>
<%@taglib prefix="t" tagdir="/WEB-INF/tags" %>
<%
/*
 * ====================================================================
 * Project:     openCRX/Core, http://www.opencrx.org/
 * Description: UserSettings
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
%><%@ page session="true" import="
java.util.*,
java.io.*,
java.text.*,
org.opencrx.kernel.generic.*,
org.opencrx.portal.wizard.*,
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
<!--
	<meta http-equiv="Content-Type" content="text/html; charset=utf-8">
-->
<%
	final String FORM_NAME = "UserSettingsForm";
	UserSettingsController wc = new UserSettingsController();
%>
	<t:wizardHandleCommand controller='<%= wc %>' defaultCommand='Refresh' />
<%
	if(response.getStatus() != HttpServletResponse.SC_OK) {
		wc.close();		
		return;
	}	
	javax.jdo.PersistenceManager pm = wc.getPm();
	ApplicationContext app = wc.getApp();
	Properties userSettings = wc.getUserSettings();
	org.opencrx.kernel.home1.jmi1.UserHome userHome = (org.opencrx.kernel.home1.jmi1.UserHome)wc.getObject();
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
	<div class="container-fluid">
		<div class="row">
			<fieldset class="col-sm">
				<div class="<%= CssClass.fieldGroupName %>">User Home</div>
				<table style="width:100%">
					<tr>
						<td style="width:300px;">
							<label style="font-weight:normal" for="timezone_sorttz">Timezone:</label> 
							<img border='0' alt='' title='sorted by timezone' height='16px' src='images/filter_down_cal.gif' />
						</td>
						<td>
							<select id="timezone_sorttz" name="timezone_sorttz" class="form-control" onchange="javascript:document.getElementById('timezone').value=this.value;document.getElementById('timezone_sortAlpha').value=this.value;" >
<%
								String initiallySelectedTZ = userSettings.getProperty(UserSettings.TIMEZONE_NAME.getName()); //TimeZone.getTimeZone(app.getCurrentTimeZone()).getDisplayName();
								String[] timezones = java.util.TimeZone.getAvailableIDs();
								for(int i = 0; i < timezones.length; i++) {
			                    	String timezoneID = timezones[i].trim();
									String selectedModifier = "";
									if (timezoneID.equals(userSettings.getProperty(UserSettings.TIMEZONE_NAME.getName()))) {
										selectedModifier = "selected";
			                    		initiallySelectedTZ = timezoneID;
									}
%>
									<option  <%= selectedModifier %> value="<%= timezoneID %>"><%= timezoneID %>
<%
								}
%>
							</select>
						</td>
					</tr>
					<tr>
						<td>
							<label style="font-weight:normal" for="timezone_sortAlpha">Timezone:</label> 
							<img border='0' alt='' title='sorted alphabetically' height='16px' src='images/filter_down_star.gif' />
						</td>
						<td>
							<select id="timezone_sortAlpha" name="timezone_sortAlpha" class="form-control" onchange="javascript:document.getElementById('timezone').value=this.value;document.getElementById('timezone_sorttz').value=this.value;" >
<%
								String[] timezonesAlpha = java.util.TimeZone.getAvailableIDs();
								java.util.Arrays.sort(timezonesAlpha, java.text.Collator.getInstance());
								for(int i = 0; i < timezonesAlpha.length; i++) {
									String timezoneID = timezonesAlpha[i].trim();
									String selectedModifier = timezoneID.equals(userSettings.getProperty(UserSettings.TIMEZONE_NAME.getName())) ? "selected" : "";
%>
	  								<option  <%= selectedModifier %> value="<%= timezoneID %>"><%= timezoneID %>
<%
								}
%>
							</select>
							<input type="text" id="timezone" name="timezone" class="form-control" value="<%= initiallySelectedTZ %>" style="position:absolute;width:0;visibility:hidden;" />
						</td>
					</tr>
					<tr>
						<td nowrap>
							<label style="font-weight:normal" for="storeSettingsOnLogoff">Store settings on logoff:</label>
						</td>
						<td>
							<input type="checkbox" <%= userHome.isStoreSettingsOnLogoff() != null && userHome.isStoreSettingsOnLogoff().booleanValue() ? "checked" : "" %> id="storeSettingsOnLogoff" name="storeSettingsOnLogoff"/>
						</td>
					</tr>
<%
					org.opencrx.kernel.home1.cci2.EMailAccountQuery emailAccountQuery = (org.opencrx.kernel.home1.cci2.EMailAccountQuery)pm.newQuery(org.opencrx.kernel.home1.jmi1.EMailAccount.class);
					emailAccountQuery.thereExistsIsActive().isTrue();
					emailAccountQuery.thereExistsIsDefault().isTrue();
					List<org.opencrx.kernel.home1.jmi1.EMailAccount> emailAccounts = userHome.getEMailAccount(emailAccountQuery);
					org.opencrx.kernel.home1.jmi1.EMailAccount defaultEmailAccount = emailAccounts.isEmpty() ? null : emailAccounts.iterator().next();
%>
					<tr>
						<td><label style="font-weight:normal" for="emailAccount">Email:</label></td>
						<td><input type="text" id="emailAccount" name="emailAccount" class="form-control" value="<%= defaultEmailAccount == null || defaultEmailAccount.getName() == null ? "" :  defaultEmailAccount.getName() %>"/></td>
					</tr>
					<tr>
						<td><label style="font-weight:normal" for="sendmailSubjectPrefix">Sendmail subject prefix:</label></td>
						<td><input type="text" id="sendmailSubjectPrefix" name="sendmailSubjectPrefix" class="form-control" value="<%= userHome.getSendMailSubjectPrefix() == null ? "[" + wc.getProviderName() + ":" + wc.getSegmentName() + "]" : userHome.getSendMailSubjectPrefix() %>"/>
					</tr>
					<tr>
						<td><label style="font-weight:normal" for="webAccessUrl">Web access URL:</label></td>
						<td><input type="text" id="webAccessUrl" name="webAccessUrl" class="form-control" value="<%= userHome.getWebAccessUrl() == null ? request.getRequestURL().substring(0, request.getRequestURL().indexOf("/wizards")) :  userHome.getWebAccessUrl()  %>"/>
					</tr>
				</table>
			</fieldset>
		</div>
		<div class="row">		
			<fieldset class="col-sm">
				<div class="<%= CssClass.fieldGroupName %>">Root Menu</div>
				<table style="width:100%">
					<tr>
						<td style="width:300px;"></td>
<%
						int p = 0;
					    for(Action action: wc.getSelectPerspectiveActions()) {
					        if(action.isEnabled()) {
					        	boolean isCurrent = (p == app.getCurrentPerspective());
%>
								<td style='text-align:center;<%= isCurrent ? "font-weight:bold;text-decoration:underline;' title='current perspective" : "" %>'><%= action.getTitle() %></td>
<%
							}
					        p++;
						}
%>
					</tr>
<%
					Action[] rootObjectActions = app.getRootObjectActions();
					// Always show root object 0
					int n = 1;
					for(int i = 1; i < rootObjectActions.length; i++) {
						Action action = rootObjectActions[i];
						if(action.getParameter(Action.PARAMETER_REFERENCE).length() == 0) {
%>
							<tr>
								<td><label style="font-weight:normal" for="rootObject<%= n %>"><%= action.getTitle() %>:</label></td>
<%		
								for(p = 0; p < wc.getSelectPerspectiveActions().length; p++) {
%>
									<td style="text-align:center;">
										<input type="checkbox" <%= userSettings.getProperty(UserSettings.ROOT_OBJECT_STATE.getName() + (p == 0 ? "" : "[" + Integer.toString(p) + "]") + "." + n + ".State", "1").equals("1") ? "checked" : "" %> id="rootObject<%= p %>_<%= n %>" name="rootObject<%= p %>_<%= n %>"/>
									</td>
<%
								}
%>
							</tr>
<%
							n++;
						}
					}
%>
					<tr>
						<td nowrap>
							<label style="font-weight:normal" for="topNavigationShowMax">Show max items in top navigation:</label>
						</td>
						<td colspan="<%= wc.getSelectPerspectiveActions().length %>">
							<input type="text" id="topNavigationShowMax" name="topNavigationShowMax" class="form-control" value="<%= userSettings.getProperty(UserSettings.TOP_NAVIGATION_SHOW_MAX.getName(), "6") %>"/>
						</td>
					</tr>
					<tr>
						<td nowrap><label style="font-weight:normal" for="hideWorkspaceDashboard">Hide workspace dashboard:</label></td>
						<td colspan="<%= wc.getSelectPerspectiveActions().length %>">
							<input type="checkbox" <%= "true".equals(userSettings.getProperty(UserSettings.HIDE_WORKSPACE_DASHBOARD.getName())) ? "checked" : "" %> id="hideWorkspaceDashboard" name="hideWorkspaceDashboard"/>
						</td>
					</tr>
					<tr>
						<td nowrap><label style="font-weight:normal" for="anchorUserDialog">Anchor user dialog:</label></td>
						<td colspan="<%= wc.getSelectPerspectiveActions().length %>">
							<input type="checkbox" <%= "true".equals(userSettings.getProperty(UserSettings.ANCHOR_USER_DIALOG.getName())) ? "checked" : "" %> id="anchorUserDialog" name="anchorUserDialog"/>
						</td>
					</tr>
				</table>
			</fieldset>
		</div>
		<div class="row">		
			<fieldset class="col-sm">
				<div class="<%= CssClass.fieldGroupName %>">Subscriptions</div>
				<table style="width:100%">
					<tr>
						<td style="width:300px;"></td>
						<td></td>
						<td colspan="3" style="background-color:#DDDDDD;text-align:center;">Notify on</td>
					</tr>
					<tr>
						<td></td>
						<td style="text-align:center;">Active</td>
						<td style="text-align:center;">Creation</td>
						<td style="text-align:center;">Replacement</td>
						<td style="text-align:center;">Removal</td>
					</tr>
<%
					org.opencrx.kernel.workflow1.jmi1.Segment workflowSegment = org.opencrx.kernel.backend.Workflows.getInstance().getWorkflowSegment(pm, wc.getProviderName(), wc.getSegmentName());
					org.opencrx.kernel.workflow1.cci2.TopicQuery topicQuery =
						(org.opencrx.kernel.workflow1.cci2.TopicQuery)pm.newQuery(org.opencrx.kernel.workflow1.jmi1.Topic.class);
					topicQuery.orderByName().ascending();
					topicQuery.forAllDisabled().isFalse();
					for(org.opencrx.kernel.workflow1.jmi1.Topic topic: workflowSegment.getTopic(topicQuery)) {
						ObjectReference objRefTopic = new ObjectReference(topic, app);
						org.opencrx.kernel.home1.cci2.SubscriptionQuery query = (org.opencrx.kernel.home1.cci2.SubscriptionQuery)pm.newQuery(org.opencrx.kernel.home1.jmi1.Subscription.class);
						query.thereExistsTopic().equalTo(topic);
						Collection<org.opencrx.kernel.home1.jmi1.Subscription> subscriptions = userHome.getSubscription(query);
						org.opencrx.kernel.home1.jmi1.Subscription subscription = subscriptions.isEmpty() 
							? null
							: (org.opencrx.kernel.home1.jmi1.Subscription)subscriptions.iterator().next();
						Set<Short> eventTypes = new HashSet<Short>();
						if(subscription != null) {
							for(Short eventType: subscription.getEventType()) {
								eventTypes.add(eventType);
							}
						}
						String topicId = topic.refGetPath().getLastSegment().toString();
%>
					<tr>
						<td nowrap title="<%= topic.getDescription() != null ? topic.getDescription() : "" %>">
							<label style="font-weight:normal"><%= objRefTopic.getTitle() %>:</label>
						</td>
						<td style="text-align:center;">
							<input type="checkbox" <%= subscription == null || !subscription.isActive() ? "" : "checked" %> id="topicIsActive-<%= topicId %>" name="topicIsActive-<%= topicId %>" />
						</td>
						<td style="text-align:center;">
							<input type="checkbox" <%= eventTypes.contains((short)1) ? "checked" : "" %> id="topicCreation-<%= topicId %>" name="topicCreation-<%= topicId %>" />
						</td>
						<td style="text-align:center;">
							<input type="checkbox" <%= eventTypes.contains((short)3) ? "checked" : "" %> id="topicReplacement-<%= topicId %>" name="topicReplacement-<%= topicId %>" />
						</td>
						<td style="text-align:center;">
							<input type="checkbox" <%= eventTypes.contains((short)4) ? "checked" : "" %> id="topicRemoval-<%= topicId %>" name="topicRemoval-<%= topicId %>" />
						</td>
					</tr>
<%
				}
%>
				</table>
			</fieldset>
		</div>
	</div>
	<div id="WaitIndicator" style="width:50px;height:24px;" class="wait">&nbsp;</div>
	<div id="SubmitArea" style="display:none;">
<%
		boolean allowApply = wc.currentUserIsAdmin() || wc.currentUserOwnsHome(userHome);
		if(allowApply) {
%>		
			<input type="submit" name="OK" id="OK.Button" class="<%= CssClass.btn.toString() %> <%= CssClass.btn_light.toString() %>" tabindex="9010" value="<%= app.getTexts().getOkTitle() %>" onclick="javascript:$('WaitIndicator').style.display='block';$('SubmitArea').style.display='none'; $('Command').value=this.name;this.name='---';" />
<%
		} else {
%>
			<div class="alert alert-warning">
				<b>NOTE:</b> Update of settings not allowed. Admin permissions required.
			</div>
<%
		}
%>			
		<input type="submit" name="Cancel" class="<%= CssClass.btn.toString() %> <%= CssClass.btn_light.toString() %>" tabindex="9020" value="<%= app.getTexts().getCancelTitle() %>" onclick="javascript:$('WaitIndicator').style.display='block';$('SubmitArea').style.display='none'; $('Command').value=this.name;" />
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
	$('WaitIndicator').style.display='none';
	$('SubmitArea').style.display='block';	
</script>
<t:wizardClose controller="<%= wc %>" />
