<%@page contentType="text/html;charset=UTF-8" language="java" pageEncoding="UTF-8" %>
<%@taglib prefix="t" tagdir="/WEB-INF/tags" %>
<%
/*
 * ====================================================================
 * Project:     openCRX/Core, http://www.opencrx.org/
 * Description: CreateActivity index.jsp
 * Owner:       CRIXP Corp., Switzerland, http://www.crixp.com
 * ====================================================================
 *
 * This software is published under the BSD license
 * as listed below.
 *
 * Copyright (c) 2015 CRIXP Corp., Switzerland
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
%><%@ page session="true" import="
java.util.*,
java.io.*,
java.text.*,
org.openmdx.kernel.id.cci.*,
org.openmdx.base.accessor.jmi.cci.*,
org.openmdx.base.exception.*,
org.openmdx.portal.servlet.*,
org.openmdx.portal.servlet.attribute.*,
org.openmdx.portal.servlet.component.*,
org.openmdx.portal.servlet.control.*,
org.openmdx.portal.servlet.action.*,
org.openmdx.portal.servlet.wizards.*,
org.openmdx.base.naming.*,
org.openmdx.kernel.log.*,
org.openmdx.kernel.id.*
" %><%!

	public static class CreateActivityController extends org.openmdx.portal.servlet.AbstractWizardController {
	
	   	public void doRefresh(
	   	) throws ServiceException {
	   		
	   	}
	   	
	   	public void doOK(
		) throws ServiceException {
			this.setExitAction(
				new ObjectReference(this.getObject(), this.getApp()).getSelectObjectAction()
			);
		}

		public void doCancel(
		) throws ServiceException {
			this.setExitAction(
				new ObjectReference(this.getObject(), this.getApp()).getSelectObjectAction()
			);
		}
		
	}

%><%
	CreateActivityController wc = new CreateActivityController();
%>
	<t:wizardHandleCommand controller='<%= wc %>' defaultCommand='Refresh' />
<%
	if(response.getStatus() != HttpServletResponse.SC_OK) {
		wc.close();
		return;
	}
	ApplicationContext app = wc.getApp();
	String wizardPath = request.getContextPath() + request.getServletPath().replace("/index.jsp", "");
%>

<link rel="import" href="wizards/CreateActivity/create-activity.html">
<link rel="import" href="js/polymer/components/paper-button/paper-button.html">
<dom-module id="create-activity-wizard">
	<template>
		<div class="OperationDialogTitle"><%= wc.getToolTip() %></div>
		<form id="form" accept-charset="UTF-8" method="POST" action="<%= wc.getServletPath() %>">
			<input type="hidden" name="<%= Action.PARAMETER_REQUEST_ID %>" value="<%= wc.getRequestId() %>" />
			<input type="hidden" id="<%= Action.PARAMETER_OBJECTXRI %>" name="<%= Action.PARAMETER_OBJECTXRI %>" value="<%= wc.getObjectIdentity().toXRI() %>" />
			<input type="hidden" id="Command" name="Command" value="" />											
			<table class="tableLayout">
				<tr>
					<td class="cellObject">
						<div class="panel" style="display:block;overflow:visible;">
							<create-activity id="createActivity" url="<%= wizardPath %>" title="<%= wc.getToolTip() %>" xri="<%= wc.getObjectIdentity().toXRI() %>" provider="<%= wc.getProviderName() %>" segment="<%= wc.getSegmentName() %>" on-complete="doOK"></create-activity>
						</div>
						<div id="SubmitArea" style="float:left;">	
		      				<paper-button raised on-tap="doCreate"><%= app.getTexts().getNewText() %></paper-button>
		      				<paper-button raised on-tap="doCancel"><%= app.getTexts().getCancelTitle() %></paper-button>
		      			</div>
					</td>
				</tr>
			</table>
		</form>
		<br />
	</template>
	<script type="text/javascript">
		HTMLImports.whenReady(function () {
        	Polymer({
				is: 'create-activity-wizard',
				doCancel: function(event) {
					this.$.Command.value = "Cancel";
					this.$.form.submit();
				},
				doCreate: function(event) {
					this.$.createActivity.doCreate();
				},
				doOK: function(event) {
					this.$.Command.value = "OK";
					this.$.xri.value = event.detail.activity.xri;
					this.$.form.submit();
				}
        	});
		});
	</script>
<dom-module>
<create-activity-wizard></create-activity-wizard>
<t:wizardClose controller="<%= wc %>" />
