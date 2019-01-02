<%@page contentType="text/html;charset=UTF-8" language="java" pageEncoding="UTF-8" %>
<%@taglib prefix="t" tagdir="/WEB-INF/tags" %>
<%
/*
 * ====================================================================
 * Project:     openCRX/Core, http://www.opencrx.org/
 * Description: CreateProduct wizard
 * Owner:       CRIXP AG, Switzerland, http://www.crixp.com
 * ====================================================================
 *
 * This software is published under the BSD license
 * as listed below.
 *
 * Copyright (c) 2005-2014, CRIXP Corp., Switzerland
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
	final String FORM_NAME = "CreateProductForm";
	final String FORM_NAME_BASEPRICE = "ProductBasePriceForm";	
	CreateProductWizardController wc = new CreateProductWizardController();
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
<form id="<%=FORM_NAME%>" name="<%=FORM_NAME%>" accept-charset="UTF-8" method="POST" action="<%=wc.getServletPath()%>">
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
	<input type="hidden" name="<%=Action.PARAMETER_REQUEST_ID%>" value="<%=wc.getRequestId()%>" />
	<input type="hidden" name="<%=Action.PARAMETER_OBJECTXRI%>" value="<%=wc.getObjectIdentity().toXRI()%>" />
	<input type="hidden" id="Command" name="Command" value="" />											
	<input type="hidden" id="DeleteProductBasePriceIndex" name="DeleteProductBasePriceIndex" value="" />											
	<table class="tableLayout">
		<tr>
			<td class="cellObject">
				<div class="panel" id="panel<%=FORM_NAME%>" style="display:block;overflow:visible;">
<%
					wc.getForms().get(FORM_NAME).paint(viewPort, null, true);
					viewPort.flush();
					// Product base prices
					int lastProductBasePriceIndex = -1;
					if(wc.getProductBasePriceCount() > 0) {
%>
						<div class="fieldGroupName">Prices</div>
						<div>&nbsp;</div>
						<table class="gridTableFull">
							<tr class="gridTableHeaderFull">
								<td />
								<td><%=app.getLabel("org:opencrx:kernel:product1:PriceLevel")%></td>
								<td><%=wc.getFieldLabel("org:opencrx:kernel:product1:AbstractProductPrice", "usage", app.getCurrentLocaleAsIndex())%></td>
								<td><%=wc.getFieldLabel("org:opencrx:kernel:product1:AbstractProductPrice", "priceCurrency", app.getCurrentLocaleAsIndex())%></td>
								<td><%=wc.getFieldLabel("org:opencrx:kernel:product1:AbstractProductPrice", "quantityFrom", app.getCurrentLocaleAsIndex())%></td>
								<td><%=wc.getFieldLabel("org:opencrx:kernel:product1:AbstractProductPrice", "quantityTo", app.getCurrentLocaleAsIndex())%></td>
								<td><%=wc.getFieldLabel("org:opencrx:kernel:product1:AbstractProductPrice", "price", app.getCurrentLocaleAsIndex())%></td>
								<td><%=wc.getFieldLabel("org:opencrx:kernel:product1:AbstractProductPrice", "discountIsPercentage", app.getCurrentLocaleAsIndex())%></td>
								<td><%=wc.getFieldLabel("org:opencrx:kernel:product1:AbstractProductPrice", "discount", app.getCurrentLocaleAsIndex())%></td>
								<td class="addon"/>
							</tr>
<%
							for(int i = 0; i < wc.getProductBasePriceCount(); i++) {
								Map<String,Object> formValues = wc.getFormFields();
								if(wc.getFormFields().get("productBasePrice.priceLevel." + i) != null) {
									lastProductBasePriceIndex = i;
									java.math.BigDecimal quantityFrom = (java.math.BigDecimal)formValues.get("productBasePrice.quantityFrom." + i);
									java.math.BigDecimal quantityTo = (java.math.BigDecimal)formValues.get("productBasePrice.quantityTo." + i);
									java.math.BigDecimal price = (java.math.BigDecimal)formValues.get("productBasePrice.price." + i);
									java.math.BigDecimal discount = (java.math.BigDecimal)formValues.get("productBasePrice.discount." + i);
									org.opencrx.kernel.product1.jmi1.PriceLevel priceLevel = 
										(org.opencrx.kernel.product1.jmi1.PriceLevel)pm.getObjectById(
											formValues.get("productBasePrice.priceLevel." + i)
										);
%>
									<tr class="gridTableRowFull">
										<td><input class="<%= CssClass.btn.toString() %> <%= CssClass.btnDefault.toString() %>" type="submit" name="DeleteProductBasePrice" value="-" onclick="javascript:$('Command').value=this.name;$('DeleteProductBasePriceIndex').value='<%= i %>'"/></td>
										<td><%= new ObjectReference(priceLevel, app).getTitle() %><input type="hidden" name="productBasePrice.priceLevel.<%= i %>" value="<%= priceLevel.refGetPath().toXRI() %>"/></td>
										<td>
<%
											for(Iterator j = ((List)formValues.get("productBasePrice.usage." + i)).iterator(); j.hasNext(); ) {
												Short usage = (Short)j.next();
%>
												<%= app.getCodes().getShortText("usageproductbaseprice", app.getCurrentLocaleAsIndex(), true, true).get(usage) %>
<%
											}
%>
											<input type="hidden" name="productBasePrice.usage.<%= i %>" value="<%= formValues.get("productBasePrice.usage." + i) %>"/>
										</td>
										<td><%= app.getCodes().getShortText("currency", app.getCurrentLocaleAsIndex(), true, true).get(formValues.get("productBasePrice.priceCurrency." + i)) %><input type="hidden" name="productBasePrice.priceCurrency.<%= i %>" value="<%= formValues.get("productBasePrice.priceCurrency." + i) %>"/></td>
										<td><input class="valueR" type="text" name="productBasePrice.quantityFrom.<%= i %>" value="<%= quantityFrom == null ? "" : quantityFrom %>"/></td>
										<td><input class="valueR" type="text" name="productBasePrice.quantityTo.<%= i %>" value="<%= quantityTo == null ? "" : quantityTo %>"/></td>
										<td><input class="valueR" type="text" name="productBasePrice.price.<%= i %>" value="<%= price == null ? "" : price %>"/></td>
										<td><input class="valueR" type="checkbox" name="productBasePrice.discountIsPercentage.<%= i %>" <%= ((Boolean)formValues.get("productBasePrice.discountIsPercentage." + i)).booleanValue() ? "checked" : "" %>"/></td>
										<td><input class="valueR" type="text" name="productBasePrice.discount.<%= i %>" value="<%= discount == null ? "" : discount %>"/></td>
										<td class="addon"/>
									</tr>
<%
							    }
							}
%>
						</table>
<%
					}
					wc.getForms().get(FORM_NAME_BASEPRICE).paint(viewPort, null, true);
					viewPort.flush();
%>
					<input type="hidden" name="ProductBasePriceCount" id="ProductBasePriceCount" value="<%= lastProductBasePriceIndex + 1 %>" />
					<input type="submit" name="AddProductBasePrice" id="AddProductBasePrice.Button" class="<%= CssClass.btn.toString() %> <%= CssClass.btnDefault.toString() %>" tabindex="9000" value="+" onclick="javascript:$('Command').value=this.name;" />
					<div class="fieldGroupName">&nbsp;</div>
				</div>
				<div id="WaitIndicator" style="float:left;width:50px;height:24px;" class="wait">&nbsp;</div>
				<div id="SubmitArea" style="float:left;display:none;">								
					<input type="submit" name="Refresh" id="Refresh.Button" class="<%= CssClass.btn.toString() %> <%= CssClass.btnDefault.toString() %>" tabindex="9040" value="Refresh" onclick="javascript:$('WaitIndicator').style.display='block';$('SubmitArea').style.display='none';$('Command').value=this.name;" />
					<input type="submit" name="OK" id="OK.Button" class="<%= CssClass.btn.toString() %> <%= CssClass.btnDefault.toString() %>" tabindex="9050" value="<%= app.getTexts().getSaveTitle() %>" onclick="javascript:$('WaitIndicator').style.display='block';$('SubmitArea').style.display='none';$('Command').value=this.name;" />
					<input type="submit" name="Cancel" class="<%= CssClass.btn.toString() %> <%= CssClass.btnDefault.toString() %>" tabindex="9060" value="<%= app.getTexts().getCancelTitle() %>" onclick="javascript:$('WaitIndicator').style.display='block';$('SubmitArea').style.display='none';$('Command').value=this.name;" />
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
