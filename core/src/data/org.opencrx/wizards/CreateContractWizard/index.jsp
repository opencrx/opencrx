<%@page contentType="text/html;charset=UTF-8" language="java" pageEncoding="UTF-8" %>
<%@taglib prefix="t" tagdir="/WEB-INF/tags" %>
<%
/*
 * ====================================================================
 * Project:     openCRX/Core, http://www.opencrx.org/
 * Description: CreateContractWizard
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
%><%@ page session="true" import="
java.util.*,
java.io.*,
java.text.*,
org.opencrx.portal.wizard.*,
org.openmdx.base.exception.*,
org.openmdx.base.text.conversion.*,
org.openmdx.kernel.id.cci.*,
org.openmdx.kernel.id.*,
org.openmdx.base.accessor.jmi.cci.*,
org.openmdx.portal.servlet.*,
org.openmdx.portal.servlet.attribute.*,
org.openmdx.portal.servlet.component.*,
org.openmdx.portal.servlet.control.*,
org.openmdx.portal.servlet.wizards.*,
org.openmdx.base.naming.*
" %>
<%
	final String FORM_NAME = "CreateContractForm";
	final String FORM_NAME_POSITION = "CreateContractPositionForm";
	final String WIZARD_NAME = "CreateContractWizard.jsp";
	CreateContractWizardController wc = new CreateContractWizardController();
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
<div class="OperationDialogTitle"><%= app.getTexts().getNewText() %>...</div>
<form id="<%=FORM_NAME%>" name="<%=FORM_NAME%>" accept-charset="UTF-8" method="POST" action="<%=wc.getServletPath()%>">
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
	<input type="hidden" name="<%=Action.PARAMETER_REQUEST_ID%>" value="<%=wc.getRequestId()%>" />
	<input type="hidden" name="<%=Action.PARAMETER_OBJECTXRI%>" value="<%=wc.getObjectIdentity().toXRI()%>" />
	<input type="hidden" id="Command" name="Command" value="Refresh" />
	<input type="hidden" id="AddressXri" name="AddressXri" value="" />
	<input type="hidden" id="DeletePositionIndex" name="DeletePositionIndex" value="" />
	<table class="tableLayout">
		<tr>
			<td class="cellObject">
				<div class="panel" id="panel<%=FORM_NAME%>" style="display:block;overflow:visible;">
<%
					// Contract
					wc.getForms().get(FORM_NAME).paint(viewPort, null, true);
					viewPort.flush();

					// Contract positions
					int lastContractPositionIndex = -1;
					if(wc.getContractPositionCount() > 0) {
%>
						<div class="fieldGroupName">Positions</div>
						<div>&nbsp;</div>
						<table class="gridTableFull">
							<tr class="gridTableHeaderFull">
								<td />
								<td><%=wc.getFieldLabel("org:opencrx:kernel:contract1:SalesContractPosition", "quantity", app.getCurrentLocaleAsIndex())%></td>
								<td><%=wc.getFieldLabel("org:opencrx:kernel:product1:ProductDescriptor", "product", app.getCurrentLocaleAsIndex())%></td>
								<td><%=wc.getFieldLabel("org:opencrx:kernel:contract1:SalesContractPosition", "name", app.getCurrentLocaleAsIndex())%></td>
								<td><%=wc.getFieldLabel("org:opencrx:kernel:contract1:SalesContractPosition", "pricePerUnit", app.getCurrentLocaleAsIndex())%></td>
								<td><%=wc.getFieldLabel("org:opencrx:kernel:contract1:SalesContractPosition", "baseAmount", app.getCurrentLocaleAsIndex())%></td>
								<td class="addon"/>
							</tr>
<%
							Map<String,Object> formValues = wc.getFormFields();
							for(int i = 0; i < wc.getContractPositionCount(); i++) {
								if(formValues.get("position.product.xri." + i) != null) {
									lastContractPositionIndex = i;
%>
									<tr class="gridTableRowFull">
										<td><input type="submit" name="DeletePosition" value="-" onclick="javascript:$('Command').value=this.name;$('DeletePositionIndex').value='<%= i %>';"/></td>
										<td><input class="valueR" type="text" name="position.quantity.<%= i %>" value="<%= formValues.get("position.quantity." + i) %>"/></td>
										<td><input class="valueL" type="text" readonly name="position.product.<%= i %>" value="<%= formValues.get("position.product." + i) %>"/><input type="hidden" name="position.product.xri.<%= i %>" value="<%= formValues.get("position.product.xri." + i) %>"/></td>
										<td><input class="valueL" type="text" name="position.name.<%= i %>" value="<%= formValues.get("position.name." + i) %>"/></td>
										<td><input class="valueR" type="text" name="position.pricePerUnit.<%= i %>" value="<%= formValues.get("position.pricePerUnit." + i) %>"/></td>
										<td><input class="valueR" type="text" readonly name="position.amount.<%= i %>" value="<%= formValues.get("position.amount." + i) %>"/></td>
										<td class="addon"/>
									</tr>
<%
							    }
							}
%>
						</table>
<%
					}
					wc.getForms().get(FORM_NAME_POSITION).paint(viewPort, null, true);
					viewPort.flush();
%>
					<input type="hidden" name="ContractPositionCount" id="ContractPositionCount" value="<%= lastContractPositionIndex + 1 %>" />
					<input type="submit" name="AddPosition" id="AddPosition.Button" class="<%= CssClass.btn.toString() %> <%= CssClass.btn_light.toString() %>" tabindex="9000" value="+" onclick="javascript:$('Command').value=this.name"/>
					<div class="fieldGroupName">&nbsp;</div>
				</div>
				<div id="WaitIndicator" style="float:left;width:50px;height:24px;" class="wait">&nbsp;</div>
				<div id="SubmitArea" style="float:left;display:none;">				
					<input type="submit" name="Refresh" id="Refresh.Button" class="<%= CssClass.btn.toString() %> <%= CssClass.btn_light.toString() %>" tabindex="9010" value="Refresh" onclick="javascript:$('Command').value=this.name;"/>
					<input type="submit" name="CreateOpportunity" id="CreateOpportunity.Button" class="<%= CssClass.btn.toString() %> <%= CssClass.btn_light.toString() %>" tabindex="9020" value="Create Opportunity" onclick="javascript:$('Command').value=this.name;" />
					<input type="submit" name="CreateQuote" id="CreateQuote.Button" class="<%= CssClass.btn.toString() %> <%= CssClass.btn_light.toString() %>" tabindex="9030" value="Create Quote" onclick="javascript:$('Command').value=this.name;" />
					<input type="submit" name="CreateSalesOrder" id="CreateSalesOrder.Button" class="<%= CssClass.btn.toString() %> <%= CssClass.btn_light.toString() %>" tabindex="9040" value="Create Sales Order" onclick="javascript:$('Command').value=this.name;" />
					<input type="submit" name="CreateInvoice" id="CreateInvoice.Button" class="<%= CssClass.btn.toString() %> <%= CssClass.btn_light.toString() %>" tabindex="9050" value="Create Invoice" onclick="javascript:$('Command').value=this.name;" />
					<input type="submit" name="Cancel" class="<%= CssClass.btn.toString() %> <%= CssClass.btn_light.toString() %>" tabindex="9010" value="<%= app.getTexts().getCancelTitle() %>" onclick="javascript:$('Command').value=this.name;" />
				</div>
<%
				if(wc.getCustomer() != null) {
%>
					<div>&nbsp;</div>
					<table class="gridTableFull">
						<tr class="gridTableHeaderFull">
							<td><%= wc.getFieldLabel("org:opencrx:kernel:contract1:AccountAddress", "address", app.getCurrentLocaleAsIndex()) %></td>
							<td>Set as shipping address</td>
							<td>Set as billing address</td>
							<td class="addon"/>
						</tr>
<%
						org.opencrx.kernel.account1.cci2.AccountAddressQuery addressQuery = (org.opencrx.kernel.account1.cci2.AccountAddressQuery)pm.newQuery(org.opencrx.kernel.account1.jmi1.PostalAddress.class);
						List<org.opencrx.kernel.account1.jmi1.AccountAddress> addresses = wc.getCustomer().getAddress(addressQuery);
						int ii = 0;
						for(Iterator<org.opencrx.kernel.account1.jmi1.AccountAddress> i = addresses.iterator(); i.hasNext(); ) {
						    org.opencrx.kernel.account1.jmi1.AccountAddress address = i.next();
%>
							<tr class="gridTableRowFull">
								<td><%= new ObjectReference(address, app).getTitle() %></td>
								<td><input type="submit" id="Button.SetShippingAddress.<%= Integer.toString(ii) %>" name="SetShippingAddress" class="<%= CssClass.btn.toString() %> <%= CssClass.btn_light.toString() %>" value="Shipping" onclick="javascript:$('Command').value=this.name;$('AddressXri').value='<%= address.refMofId() %>';"/>
								<td><input type="submit" id="Button.SetBillingAddress.<%= Integer.toString(ii) %>" name="SetBillingAddress" class="<%= CssClass.btn.toString() %> <%= CssClass.btn_light.toString() %>" value="Billing" onclick="javascript:$('Command').value=this.name;$('AddressXri').value='<%= address.refMofId() %>';"/>
								<td class="addon"/>
							</tr>
<%
							ii++;
						}
%>
					</table>
<%
				}
%>
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

