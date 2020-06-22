<%@page contentType= "text/html;charset=utf-8" language="java" pageEncoding= "UTF-8" %>
<%@taglib prefix="t" tagdir="/WEB-INF/tags" %>
<%
/*
 * ====================================================================
 * Project:     openCRX/Core, http://www.opencrx.org/
 * Description: Merge members
 * Owner:       CRIXP Corp., Switzerland, http://www.crixp.com
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
	final String FORM_NAME = "MergeMembers";	
	MergeMembersController wc = new MergeMembersController();
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
<form name="<%= FORM_NAME %>" id="<%= FORM_NAME %>" accept-charset="UTF-8" method="POST" action="<%= wc.getServletPath() %>">
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
	<input type="hidden" name="<%= Action.PARAMETER_OBJECTXRI %>" value="<%= wc.getObjectIdentity().toXRI() %>" />
	<input type="hidden" name="<%= Action.PARAMETER_REQUEST_ID %>" value="<%= wc.getRequestId() %>" />
	<input type="hidden" id="Command" name="Command" value="" />  
	<table class="fieldGroup">
		<tr>
			<td class="<%= CssClass.fieldLabel %>"><span class="nw">&nbsp;</span></td>
			<td></td>
			<td class="addon"></td>
			<td class="<%= CssClass.fieldLabel %>"><span class="nw">&nbsp;</span></td>
			<td></td>
			<td class="addon"></td>
		</tr>
		<tr>
			<td class="<%= CssClass.fieldLabel %>"><span class="nw">Source <%= app.getLabel(MergeMembersController.ACCOUNT_CLASS) %>:</span></td>
			<td colspan=4><%= (new ObjectReference(wc.getObject(), app)).getTitle() %></td>
			<td class="addon"></td>
		</tr>
		<tr>
			<td class="<%= CssClass.fieldLabel %>"><span class="nw">Target <%= app.getLabel(MergeMembersController.ACCOUNT_CLASS) %>:</span></td>
<%
			String lookupId = org.opencrx.kernel.backend.Base.getInstance().getUidAsString();
			Action findAccountTargetObjectAction = Action.getFindObjectAction(MergeMembersController.ACCOUNT_REFERENCE, lookupId);
			String accountName = app.getLabel(MergeMembersController.ACCOUNT_CLASS);
%>
			<td colspan=4>
				<div class="autocompleterMenu">
					<ul id="<%=CssClass.ssf_nav %>" class="<%=CssClass.ssf_nav %>" onmouseover="sfinit(this);" >
					  <li><a href="#"><img border="0" alt="" src="./images/autocomplete_select.png" /></a>
					    <ul onclick="this.style.left='-999em';" onmouseout="this.style.left='';">
					      <li class="selected"><a href="#" onclick="javascript:navSelect(this);ac_addObject0.url= './'+getEncodedHRef(['./ObjectInspectorServlet', 'event', '40', 'parameter', 'xri*(xri:@openmdx:org.opencrx.kernel.account1/provider/<%= wc.getProviderName() %>/segment/<%= wc.getSegmentName() %>)*referenceName*(account)*filterByType*(org:opencrx:kernel:account1:Account)*filterByFeature*(fullName)*filterOperator*(IS_LIKE)*orderByFeature*(fullName)*position*(0)*size*(20)']);return false;"><span>&nbsp;&nbsp;&nbsp;</span><%= accountName %> / <%= wc.getFieldLabel(MergeMembersController.ACCOUNT_CLASS, "fullName", app.getCurrentLocaleAsIndex()) %></a></li>
					      <li><a href="#" onclick="javascript:navSelect(this);ac_addObject0.url= './'+getEncodedHRef(['./ObjectInspectorServlet', 'event', '40', 'parameter', 'xri*(xri:@openmdx:org.opencrx.kernel.account1/provider/<%= wc.getProviderName() %>/segment/<%= wc.getSegmentName() %>)*referenceName*(account)*filterByType*(org:opencrx:kernel:account1:Account)*filterByFeature*(description)*filterOperator*(IS_LIKE)*orderByFeature*(description)*position*(0)*size*(20)']);return false;"><span>&nbsp;&nbsp;&nbsp;</span><%= accountName %> / <%= wc.getFieldLabel(MergeMembersController.ACCOUNT_CLASS, "description", app.getCurrentLocaleAsIndex()) %></a></li>
					      <li><a href="#" onclick="javascript:navSelect(this);ac_addObject0.url= './'+getEncodedHRef(['./ObjectInspectorServlet', 'event', '40', 'parameter', 'xri*(xri:@openmdx:org.opencrx.kernel.account1/provider/<%= wc.getProviderName() %>/segment/<%= wc.getSegmentName() %>)*referenceName*(account)*filterByType*(org:opencrx:kernel:account1:Account)*filterByFeature*(aliasName)*filterOperator*(IS_LIKE)*orderByFeature*(aliasName)*position*(0)*size*(20)']);return false;"><span>&nbsp;&nbsp;&nbsp;</span><%= accountName %> / <%= wc.getFieldLabel(MergeMembersController.ACCOUNT_CLASS, "aliasName", app.getCurrentLocaleAsIndex()) %></a></li>
					      <li><a href="#" onclick="javascript:navSelect(this);ac_addObject0.url= './'+getEncodedHRef(['./ObjectInspectorServlet', 'event', '40', 'parameter', 'xri*(xri:@openmdx:org.opencrx.kernel.account1/provider/<%= wc.getProviderName() %>/segment/<%= wc.getSegmentName() %>)*referenceName*(account)*filterByType*(org:opencrx:kernel:account1:Account)*filterByFeature*(firstName)*filterOperator*(IS_LIKE)*orderByFeature*(firstName)*position*(0)*size*(20)']);return false;"><span>&nbsp;&nbsp;&nbsp;</span><%= accountName %> / <%= wc.getFieldLabel(MergeMembersController.CONTACT_CLASS, "firstName", app.getCurrentLocaleAsIndex()) %></a></li>
					      <li><a href="#" onclick="javascript:navSelect(this);ac_addObject0.url= './'+getEncodedHRef(['./ObjectInspectorServlet', 'event', '40', 'parameter', 'xri*(xri:@openmdx:org.opencrx.kernel.account1/provider/<%= wc.getProviderName() %>/segment/<%= wc.getSegmentName() %>)*referenceName*(account)*filterByType*(org:opencrx:kernel:account1:Account)*filterByFeature*(middleName)*filterOperator*(IS_LIKE)*orderByFeature*(middleName)*position*(0)*size*(20)']);return false;"><span>&nbsp;&nbsp;&nbsp;</span><%= accountName %> / <%= wc.getFieldLabel(MergeMembersController.CONTACT_CLASS, "middleName", app.getCurrentLocaleAsIndex()) %></a></li>
					      <li><a href="#" onclick="javascript:navSelect(this);ac_addObject0.url= './'+getEncodedHRef(['./ObjectInspectorServlet', 'event', '40', 'parameter', 'xri*(xri:@openmdx:org.opencrx.kernel.account1/provider/<%= wc.getProviderName() %>/segment/<%= wc.getSegmentName() %>)*referenceName*(account)*filterByType*(org:opencrx:kernel:account1:Account)*filterByFeature*(lastName)*filterOperator*(IS_LIKE)*orderByFeature*(lastName)*position*(0)*size*(20)']);return false;"><span>&nbsp;&nbsp;&nbsp;</span><%= accountName %> / <%= wc.getFieldLabel(MergeMembersController.CONTACT_CLASS, "lastName", app.getCurrentLocaleAsIndex()) %></a></li>
					      <li><a href="#" onclick="javascript:navSelect(this);ac_addObject0.url= './'+getEncodedHRef(['./ObjectInspectorServlet', 'event', '40', 'parameter', 'xri*(xri:@openmdx:org.opencrx.kernel.account1/provider/<%= wc.getProviderName() %>/segment/<%= wc.getSegmentName() %>)*referenceName*(account)*filterByType*(org:opencrx:kernel:account1:Account)*filterByFeature*(name)*filterOperator*(IS_LIKE)*orderByFeature*(name)*position*(0)*size*(20)']);return false;"><span>&nbsp;&nbsp;&nbsp;</span><%= accountName %> / <%= wc.getFieldLabel(MergeMembersController.GROUP_CLASS, "name", app.getCurrentLocaleAsIndex()) %></a></li>
					      <li><a href="#" onclick="javascript:navSelect(this);ac_addObject0.url= './'+getEncodedHRef(['./ObjectInspectorServlet', 'event', '40', 'parameter', 'xri*(xri:@openmdx:org.opencrx.kernel.account1/provider/<%= wc.getProviderName() %>/segment/<%= wc.getSegmentName() %>)*referenceName*(account)*filterByType*(org:opencrx:kernel:account1:Account)*filterByFeature*(nickName)*filterOperator*(IS_LIKE)*orderByFeature*(nickName)*position*(0)*size*(20)']);return false;"><span>&nbsp;&nbsp;&nbsp;</span><%= accountName %> / <%= wc.getFieldLabel(MergeMembersController.CONTACT_CLASS, "nickName", app.getCurrentLocaleAsIndex()) %></a></li>
					    </ul>
					  </li>
					</ul>
				</div>
				<div class="autocompleterInput"><input type="text" class="valueL valueAC <%= "OK".equals(wc.getCommand()) && (wc.getAccountTarget() == null) ? "mandatory" : "" %>" id="accountTargetXri.Title" name="accountTargetXriTitle" tabindex="100" value="<%= wc.getAccountTargetXriTitle() != null ? wc.getAccountTargetXriTitle() : "" %>" /></div>
				<input type="hidden" class="valueLLocked" id="accountTargetXri" name="accountTargetXri" readonly value="<%= wc.getAccountTargetXri() != null ? wc.getAccountTargetXri().toXRI() : "" %>" />
				<div class="autocomplete" id="accountTargetXri.Update" style="display:none;z-index:500;"></div>
				<script type="text/javascript" charset="utf-8">
					ac_addObject0 = new Ajax.Autocompleter(
					  'accountTargetXri.Title',
					  'accountTargetXri.Update',
					  './ObjectInspectorServlet?event=40&parameter=xri*%28xri%3A%40openmdx%3Aorg.opencrx.kernel.account1%2Fprovider%2F<%= wc.getProviderName() %>%2Fsegment%2F<%= wc.getSegmentName() %>%29*referenceName*%28account%29*filterByType*%28org%3Aopencrx%3Akernel%3Aaccount1%3AAccount%29*filterByFeature*%28fullName%29*filterOperator*%28IS_LIKE%29*orderByFeature*%28fullName%29*position*%280%29*size*%2820%29',
					  {
					    paramName: 'filtervalues',
					    minChars: 0,
					    afterUpdateElement: updateXriField
					  }
					);
				</script>
			</td>
			<td class="addon">
			   <img class="popUpButton" border="0" align="bottom" alt="Click to open ObjectFinder" src="./images/lookup.gif" onclick="OF.findObject('./<%= findAccountTargetObjectAction.getEncodedHRef() %>', $('accountTargetXri.Title'), $('accountTargetXri'), '<%= lookupId %>');" />
			</td>
		</tr>
		<tr>
		   <td class="<%= CssClass.fieldLabel %>">Add disabled members:</td>
		  <td colspan=4><input type="checkbox" name="includeDisabledMembers" tabindex="500" value="true" <%= Boolean.TRUE.equals(wc.getFormFields().getIncludeDisabledMembers()) ? "checked" : "" %> /></td>
		  <td class="addon"></td>
		</tr>
		<tr>
		   <td class="<%= CssClass.fieldLabel %>">Add disabled accounts:</td>
		  <td colspan=4><input type="checkbox" name="includeDisabledAccounts" tabindex="500" value="true" <%= Boolean.TRUE.equals(wc.getFormFields().getIncludeDisabledAccounts()) ? "checked" : "" %> /></td>
		  <td class="addon"></td>
		</tr>
	</table>
	<br>
	<div id="WaitIndicator" style="float:left;width:50px;height:24px;" class="wait">&nbsp;</div>
	<div id="SubmitArea" style="float:left;display:none;">				
		<input type="submit" name="OK" class="<%= CssClass.btn.toString() %> <%= CssClass.btn_light.toString() %>" tabindex="9000" value="<%= app.getTexts().getOkTitle() %>" onclick="javascript:$('WaitIndicator').style.display='block';$('SubmitArea').style.display='none'; $('Command').value=this.name;" />
		<input type="submit" name="Cancel" class="<%= CssClass.btn.toString() %> <%= CssClass.btn_light.toString() %>" tabindex="9010" value="<%= app.getTexts().getCancelTitle() %>" onClick="javascript:$('WaitIndicator').style.display='block';$('SubmitArea').style.display='none'; $('Command').value=this.name;" />
	</div>
	<br />
<%
	if("OK".equals(wc.getCodes()) && wc.getAccountTarget() != null) {
		String accountTargetHref = "";
		Action action = new ObjectReference(
		  wc.getAccountTarget(),
		  app
		).getSelectObjectAction();
		accountTargetHref = action.getEncodedHRef();
%>
		<br />
		<table><tr><td>
			<table id="resultTable" class="gridTableFull">
				<tr class="gridTableHeaderFull"><!-- 4 columns -->
				  <td><b><%= app.getLabel(MergeMembersController.MEMBER_CLASS) %></b></td>
				  <td align=center>SOURCE<br><b><%= app.getLabel(MergeMembersController.MEMBER_CLASS) %> <%= wc.getFieldLabel(MergeMembersController.MEMBER_CLASS, "disabled", app.getCurrentLocaleAsIndex()) %></b></td>
				  <td align=center>SOURCE<br><b><%= app.getLabel(MergeMembersController.ACCOUNT_CLASS) %> <%= wc.getFieldLabel(MergeMembersController.ACCOUNT_CLASS, "disabled", app.getCurrentLocaleAsIndex()) %></b></td>
				  <td align=center>Added to TARGET<br><b><a href="<%= accountTargetHref %>" target="_blank"><%= wc.getAccountTarget().getFullName() %></a></b></td>
				</tr>
<%
				for(MergeMembersController.MergedMember mergedMember: wc.getMergedMembers()) {
%>
					<tr class="gridTableRowFull">
						<td><a href="<%= mergedMember.getMemberHref() %>" target="_blank"><%= mergedMember.getInfo() %></a></td>
						<td align=center><img src="./images/<%= mergedMember.getMember() != null && Boolean.TRUE.equals(mergedMember.getMember().isDisabled()) ? "" : "not" %>checked_r.gif" alt="" /></td>
						<td align=center><img src="./images/<%= mergedMember.getAccount() != null && Boolean.TRUE.equals(mergedMember.getAccount().isDisabled()) ? "" : "not" %>checked_r.gif" alt="" /></td>
<%
						if(mergedMember.getMember() != null) {
%>
							<td align=center><img src="./images/checked_r.gif" alt="" /></td>
<%
						} else {
%>
							<td align=center><img src="./images/notchecked_r.gif" alt="" /></td>
<%
						}
%>
					</tr>
<%
				}
%>
			</table>
		</td></tr></table>
<%
	}
%>
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
