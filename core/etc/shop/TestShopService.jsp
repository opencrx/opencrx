<%@  page contentType="text/html;charset=UTF-8" language="java" pageEncoding="UTF-8" %><%
/*
 * ====================================================================
 * Project:     openCRX/Core, http://www.opencrx.org/
 * Name:        $Id: TestShopService.jsp,v 1.7 2012/05/23 16:34:07 wfro Exp $
 * Description: TestShopService
 * Revision:    $Revision: 1.7 $
 * Owner:       CRIXP AG, Switzerland, http://www.crixp.com
 * Date:        $Date: 2012/05/23 16:34:07 $
 * ====================================================================
 *
 * This software is published under the BSD license
 * as listed below.
 *
 * Copyright (c) 2005-2009, CRIXP Corp., Switzerland
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
java.net.*,
java.text.*,
org.openmdx.base.accessor.jmi.cci.*,
org.openmdx.portal.servlet.*,
org.openmdx.portal.servlet.attribute.*,org.openmdx.portal.servlet.component.*,
org.openmdx.portal.servlet.texts.*,
org.openmdx.portal.servlet.control.*,
org.openmdx.portal.servlet.reports.*,
org.openmdx.portal.servlet.wizards.*,
org.openmdx.base.naming.*,
org.opencrx.kernel.backend.*,
org.openmdx.kernel.id.cci.*,
org.openmdx.kernel.id.*,
org.openmdx.base.exception.*
" %><%

	final String WIZARD_NAME = "TestShopService.jsp";

	// Init
	request.setCharacterEncoding("UTF-8");
	ApplicationContext app = (ApplicationContext)session.getValue(WebKeys.APPLICATION_KEY);
	ViewsCache viewsCache = (ViewsCache)session.getValue(WebKeys.VIEW_CACHE_KEY_SHOW);
	String requestId =  request.getParameter(Action.PARAMETER_REQUEST_ID);
	String objectXri = request.getParameter(Action.PARAMETER_OBJECTXRI);
	if(objectXri == null || app == null || viewsCache.getView(requestId) == null) {
		response.sendRedirect(
			request.getContextPath() + "/" + WebKeys.SERVLET_NAME
		);
		return;
	}
	javax.jdo.PersistenceManager pm = app.getPmData();
	RefObject_1_0 obj = (RefObject_1_0)pm.getObjectById(new Path(objectXri));
	Texts_1_0 texts = app.getTexts();
	org.openmdx.portal.servlet.Codes codes = app.getCodes();

	// Get Parameters
	String command = request.getParameter("command");
	
	String providerName = obj.refGetPath().get(2);
	String segmentName = obj.refGetPath().get(4);

	org.opencrx.application.shop1.cci2.ReturnStatusT returnStatus = null;

	org.opencrx.application.shop1.test.TestShopService shopServiceTester = 
		new org.opencrx.application.shop1.test.TestShopService(
			new org.opencrx.application.shop1.service.ShopServiceImpl(
				pm,
				providerName,
				segmentName,
				"TestShop",
				true, // emailAddressMustBeUnique
				false, // ignoreProductConfiguration
				new org.opencrx.application.shop1.datatypes.DatatypeMappers()
			)
		);

	// Cancel
	if("Cancel".equalsIgnoreCase(command)) {
		session.setAttribute(WIZARD_NAME, null);
		Action nextAction = new ObjectReference(obj, app).getSelectObjectAction();
		response.sendRedirect(
			request.getContextPath() + "/" + nextAction.getEncodedHRef()
		);
		return;
	}
	// TestCustomers
	else if("TestCustomers".equals(command)) {
		returnStatus = shopServiceTester.testCustomers();
	}
	// TestCustomers10
	else if("TestCustomers10".equals(command)) {
		for(int i = 0; i < 10; i++) {
			returnStatus = shopServiceTester.testCustomers();
		}
	}
	// TestDocuments
	else if("TestDocuments".equals(command)) {
		returnStatus = shopServiceTester.testDocuments();
	}
	// TestDocuments10
	else if("TestDocuments10".equals(command)) {
		for(int i = 0; i < 10; i++) {
			returnStatus = shopServiceTester.testDocuments();
		}
	}
	// TestLegalEntities
	else if("TestLegalEntities".equals(command)) {
		returnStatus = shopServiceTester.testLegalEntities();
	}
	// TestLegalEntities10
	else if("TestLegalEntities10".equals(command)) {
		for(int i = 0; i < 10; i++) {
			returnStatus = shopServiceTester.testLegalEntities();
		}
	}
	// TestProducts
	else if("TestProducts".equals(command)) {
		returnStatus = shopServiceTester.testProducts();
	}
	// TestProducts10
	else if("TestProducts10".equals(command)) {
		for(int i = 0; i < 10; i++) {
			returnStatus = shopServiceTester.testProducts();
		}
	}
	// TestSalesOrders
	else if("TestSalesOrders".equals(command)) {
		returnStatus = shopServiceTester.testSalesOrders();
	}
	// TestSalesOrders10
	else if("TestSalesOrders10".equals(command)) {
		for(int i = 0; i < 10; i++) {
			returnStatus = shopServiceTester.testSalesOrders();
		}
	}
	// TestInvoices
	else if("TestInvoices".equals(command)) {
		returnStatus = shopServiceTester.testInvoices();
	}
	// TestInvoices10
	else if("TestInvoices10".equals(command)) {
		for(int i = 0; i < 10; i++) {
			returnStatus = shopServiceTester.testInvoices();
		}
	}
	// TestVouchers
	else if("TestVouchers".equals(command)) {
		returnStatus = shopServiceTester.testVouchers();
	}
	// TestVouchers10
	else if("TestVouchers10".equals(command)) {
		for(int i = 0; i < 10; i++) {
			returnStatus = shopServiceTester.testVouchers();
		}
	}
	// TestCodeValues
	else if("TestCodeValues".equals(command)) {
		returnStatus = shopServiceTester.testCodeValues();
	}
	// TestCodeValues10
	else if("TestCodeValues10".equals(command)) {
		for(int i = 0; i < 10; i++) {
			returnStatus = shopServiceTester.testCodeValues();
		}
	}
	// TestActivities
	else if("TestActivities".equals(command)) {
		returnStatus = shopServiceTester.testActivities();
	}
	// TestActivities10
	else if("TestActivities10".equals(command)) {
		for(int i = 0; i < 10; i++) {
			returnStatus = shopServiceTester.testActivities();
		}
	}
	// TestRegisterCustomer
	else if("TestRegisterCustomer".equals(command)) {
		returnStatus = shopServiceTester.testRegisterCustomer();
	}
	// TestRegisterCustomer10
	else if("TestRegisterCustomer10".equals(command)) {
		for(int i = 0; i < 10; i++) {
			returnStatus = shopServiceTester.testRegisterCustomer();
		}
	}
	// TestAll
	else if("TestAll".equals(command)) {
		returnStatus = shopServiceTester.testProducts();
		returnStatus = shopServiceTester.testCustomers();
		returnStatus = shopServiceTester.testDocuments();
		returnStatus = shopServiceTester.testLegalEntities();
		returnStatus = shopServiceTester.testSalesOrders();
		returnStatus = shopServiceTester.testInvoices();
		returnStatus = shopServiceTester.testVouchers();
		returnStatus = shopServiceTester.testCodeValues();
		returnStatus = shopServiceTester.testActivities();
		returnStatus = shopServiceTester.testRegisterCustomer();
	}
	// TestAll10
	else if("TestAll10".equals(command)) {
		for(int i = 0; i < 10; i++) {
			returnStatus = shopServiceTester.testProducts();
			returnStatus = shopServiceTester.testCustomers();
			returnStatus = shopServiceTester.testDocuments();
			returnStatus = shopServiceTester.testLegalEntities();
			returnStatus = shopServiceTester.testSalesOrders();
			returnStatus = shopServiceTester.testInvoices();
			returnStatus = shopServiceTester.testVouchers();
			returnStatus = shopServiceTester.testCodeValues();
			returnStatus = shopServiceTester.testActivities();
			returnStatus = shopServiceTester.testRegisterCustomer();
		}
	}
	// TestAllBuy
	else if("TestAllBuy".equals(command)) {
		returnStatus = shopServiceTester.testSalesOrders();
		returnStatus = shopServiceTester.testInvoices();
		returnStatus = shopServiceTester.testVouchers();
	}
	// TestAllBuy10
	else if("TestAllBuy10".equals(command)) {
		for(int i = 0; i < 10; i++) {
			returnStatus = shopServiceTester.testSalesOrders();
			returnStatus = shopServiceTester.testInvoices();
			returnStatus = shopServiceTester.testVouchers();
		}
	}
	// TestCreate100Products
	else if("TestCreate100Products".equals(command)) {
		returnStatus = shopServiceTester.testCreate100Products();
	}
	// TestCreate100Products10
	else if("TestCreate100Products10".equals(command)) {
		for(int i = 0; i < 10; i++) {
			returnStatus = shopServiceTester.testCreate100Products();
		}
	}
	response.setCharacterEncoding("UTF-8");
%>
<!--[if IE]><!DOCTYPE html PUBLIC "-//W3C//DTD HTML 4.01 Transitional//EN"><![endif]-->
<!DOCTYPE HTML PUBLIC "-//W3C//DTD HTML 4.01 Transitional//EN" "http://www.w3.org/TR/html4/loose.dtd">
<html dir="<%= texts.getDir() %>">
<head>
	<style type="text/css" media="all">
		body{font-family: Arial, Helvetica, sans-serif; padding: 0; margin:0;}
		h1{  margin: 0; padding: 0 1em; font-size: 150%;}
		h2{ font-size: 130%; margin: 0; text-align: center;}

		a{text-decoration: none;}
		img{border: none;}

		/* Main Navigation across the top */
		.nav{padding: 0; margin: 0 0 1em 0; }
		.nav li{display: inline; }
		.nav a{padding: 0 0.5em; border: 1px solid silver;}
		.nav a:hover,
		.nav a:focus{background-color: silver; border: 1px solid gray;}
		.nav.secondary {float: right;}

		#content{width: 80%; margin: 0 auto; font-size: 90%;}

		fieldset{
			margin: 1%;
			padding: 1%;
			-moz-border-radius: 10px;
			border: 1.5px solid #DDD;
			background-color: #EEE;}
		legend{
			border: 1px solid #CCC;
			-moz-border-radius: 10px;
			padding: 0 1em;
			background-color: #CCC;
		}
		textarea,
		input[type='text'],
		input[type='password']{
			width: 100%;
			margin: 0; border: 1px solid silver;
			padding: 0;
			font-size: 100%;
			font-family: Arial, Helvetica, sans-serif;
		}

		input.button{
			-moz-border-radius: 4px;
			width: 120px;
			border: 1px solid silver;
		}

		/* Add/Edit page specific settings */
		.col1,
		.col2{float: left; width: 49.5%;}

		.buttons{clear: both; text-align: right;}
		table{border-collapse: collapse; width: 100%; clear: both;}
		tr{}

		/* List page specific settings */
		table.listview tr{
			border: 1px solid #36c;
			border-style: solid none;
		}
		table.listview tr:hover{
			background-color: #F0F0F0;
		}

	</style>
	<title>openCRX - Test Shop Service</title>
	<meta name="label" content="TestShopService">
	<meta name="toolTip" content="TestShopService">
	<meta name="targetType" content="_self">
	<meta name="forClass" content="org:opencrx:kernel:account1:Segment">
	<meta http-equiv="Content-Type" content="text/html; charset=utf-8">
	<link href="../../_style/n2default.css" rel="stylesheet" type="text/css">
</head>
<body>
	<ul class="<%=CssClass.ssfNav %>">
	</ul>
	<div class="col1">
		<!-- TestCustomers -->
		<form method="post" action="<%= WIZARD_NAME %>">
			<input type="hidden" name="command" value="TestCustomers"/>
			<input type="hidden" name="<%= Action.PARAMETER_REQUEST_ID %>" value="<%= requestId %>" />
			<input type="hidden" name="<%= Action.PARAMETER_OBJECTXRI %>" value="<%= objectXri %>" />
			<div class="col1">
				<fieldset>
					<legend>TestCustomers</legend>
					<input type="submit" value="Test" class="button" />
				</fieldset>
			</div>
		</form>
		<!-- TestCustomers10 -->
		<form method="post" action="<%= WIZARD_NAME %>">
			<input type="hidden" name="command" value="TestCustomers10"/>
			<input type="hidden" name="<%= Action.PARAMETER_REQUEST_ID %>" value="<%= requestId %>" />
			<input type="hidden" name="<%= Action.PARAMETER_OBJECTXRI %>" value="<%= objectXri %>" />
			<div class="col1">
				<fieldset>
					<legend>TestCustomers10</legend>
					<input type="submit" value="Test" class="button" />
				</fieldset>
			</div>
		</form>
		<!-- TestDocuments -->
		<form method="post" action="<%= WIZARD_NAME %>">
			<input type="hidden" name="command" value="TestDocuments"/>
			<input type="hidden" name="<%= Action.PARAMETER_REQUEST_ID %>" value="<%= requestId %>" />
			<input type="hidden" name="<%= Action.PARAMETER_OBJECTXRI %>" value="<%= objectXri %>" />
			<div class="col1">
				<fieldset>
					<legend>TestDocuments</legend>
					<input type="submit" value="Test" class="button" />
				</fieldset>
			</div>
		</form>
		<!-- TestDocuments10 -->
		<form method="post" action="<%= WIZARD_NAME %>">
			<input type="hidden" name="command" value="TestDocuments10"/>
			<input type="hidden" name="<%= Action.PARAMETER_REQUEST_ID %>" value="<%= requestId %>" />
			<input type="hidden" name="<%= Action.PARAMETER_OBJECTXRI %>" value="<%= objectXri %>" />
			<div class="col1">
				<fieldset>
					<legend>TestDocuments10</legend>
					<input type="submit" value="Test" class="button" />
				</fieldset>
			</div>
		</form>
		<!-- TestLegalEntities -->
		<form method="post" action="<%= WIZARD_NAME %>">
			<input type="hidden" name="command" value="TestLegalEntities"/>
			<input type="hidden" name="<%= Action.PARAMETER_REQUEST_ID %>" value="<%= requestId %>" />
			<input type="hidden" name="<%= Action.PARAMETER_OBJECTXRI %>" value="<%= objectXri %>" />
			<div class="col1">
				<fieldset>
					<legend>TestLegalEntities</legend>
					<input type="submit" value="Test" class="button" />
				</fieldset>
			</div>
		</form>
		<!-- TestLegalEntities10 -->
		<form method="post" action="<%= WIZARD_NAME %>">
			<input type="hidden" name="command" value="TestLegalEntities10"/>
			<input type="hidden" name="<%= Action.PARAMETER_REQUEST_ID %>" value="<%= requestId %>" />
			<input type="hidden" name="<%= Action.PARAMETER_OBJECTXRI %>" value="<%= objectXri %>" />
			<div class="col1">
				<fieldset>
					<legend>TestLegalEntities10</legend>
					<input type="submit" value="Test" class="button" />
				</fieldset>
			</div>
		</form>
		<!-- TestProducts -->
		<form method="post" action="<%= WIZARD_NAME %>">
			<input type="hidden" name="command" value="TestProducts"/>
			<input type="hidden" name="<%= Action.PARAMETER_REQUEST_ID %>" value="<%= requestId %>" />
			<input type="hidden" name="<%= Action.PARAMETER_OBJECTXRI %>" value="<%= objectXri %>" />
			<div class="col1">
				<fieldset>
					<legend>TestProducts</legend>
					<input type="submit" value="Test" class="button" />
				</fieldset>
			</div>
		</form>	
		<!-- TestProducts10 -->
		<form method="post" action="<%= WIZARD_NAME %>">
			<input type="hidden" name="command" value="TestProducts10"/>
			<input type="hidden" name="<%= Action.PARAMETER_REQUEST_ID %>" value="<%= requestId %>" />
			<input type="hidden" name="<%= Action.PARAMETER_OBJECTXRI %>" value="<%= objectXri %>" />
			<div class="col1">
				<fieldset>
					<legend>TestProducts10</legend>
					<input type="submit" value="Test" class="button" />
				</fieldset>
			</div>
		</form>	
		<!-- TestSalesOrders -->
		<form method="post" action="<%= WIZARD_NAME %>">
			<input type="hidden" name="command" value="TestSalesOrders"/>
			<input type="hidden" name="<%= Action.PARAMETER_REQUEST_ID %>" value="<%= requestId %>" />
			<input type="hidden" name="<%= Action.PARAMETER_OBJECTXRI %>" value="<%= objectXri %>" />
			<div class="col1">
				<fieldset>
					<legend>TestSalesOrders</legend>
					<input type="submit" value="Test" class="button" />
				</fieldset>
			</div>
		</form>	
		<!-- TestSalesOrders10 -->
		<form method="post" action="<%= WIZARD_NAME %>">
			<input type="hidden" name="command" value="TestSalesOrders10"/>
			<input type="hidden" name="<%= Action.PARAMETER_REQUEST_ID %>" value="<%= requestId %>" />
			<input type="hidden" name="<%= Action.PARAMETER_OBJECTXRI %>" value="<%= objectXri %>" />
			<div class="col1">
				<fieldset>
					<legend>TestSalesOrders10</legend>
					<input type="submit" value="Test" class="button" />
				</fieldset>
			</div>
		</form>	
		<!-- TestInvoices -->
		<form method="post" action="<%= WIZARD_NAME %>">
			<input type="hidden" name="command" value="TestInvoices"/>
			<input type="hidden" name="<%= Action.PARAMETER_REQUEST_ID %>" value="<%= requestId %>" />
			<input type="hidden" name="<%= Action.PARAMETER_OBJECTXRI %>" value="<%= objectXri %>" />
			<div class="col1">
				<fieldset>
					<legend>TestInvoices</legend>
					<input type="submit" value="Test" class="button" />
				</fieldset>
			</div>
		</form>	
		<!-- TestInvoices10 -->
		<form method="post" action="<%= WIZARD_NAME %>">
			<input type="hidden" name="command" value="TestInvoices10"/>
			<input type="hidden" name="<%= Action.PARAMETER_REQUEST_ID %>" value="<%= requestId %>" />
			<input type="hidden" name="<%= Action.PARAMETER_OBJECTXRI %>" value="<%= objectXri %>" />
			<div class="col1">
				<fieldset>
					<legend>TestInvoices10</legend>
					<input type="submit" value="Test" class="button" />
				</fieldset>
			</div>
		</form>	
		<!-- TestCodeValues -->
		<form method="post" action="<%= WIZARD_NAME %>">
			<input type="hidden" name="command" value="TestCodeValues"/>
			<input type="hidden" name="<%= Action.PARAMETER_REQUEST_ID %>" value="<%= requestId %>" />
			<input type="hidden" name="<%= Action.PARAMETER_OBJECTXRI %>" value="<%= objectXri %>" />
			<div class="col1">
				<fieldset>
					<legend>TestCodeValues</legend>
					<input type="submit" value="Test" class="button" />
				</fieldset>
			</div>
		</form>	
		<!-- TestCodeValues10 -->
		<form method="post" action="<%= WIZARD_NAME %>">
			<input type="hidden" name="command" value="TestCodeValues10"/>
			<input type="hidden" name="<%= Action.PARAMETER_REQUEST_ID %>" value="<%= requestId %>" />
			<input type="hidden" name="<%= Action.PARAMETER_OBJECTXRI %>" value="<%= objectXri %>" />
			<div class="col1">
				<fieldset>
					<legend>TestCodeValues10</legend>
					<input type="submit" value="Test" class="button" />
				</fieldset>
			</div>
		</form>	
		<!-- TestActivities -->
		<form method="post" action="<%= WIZARD_NAME %>">
			<input type="hidden" name="command" value="TestActivities"/>
			<input type="hidden" name="<%= Action.PARAMETER_REQUEST_ID %>" value="<%= requestId %>" />
			<input type="hidden" name="<%= Action.PARAMETER_OBJECTXRI %>" value="<%= objectXri %>" />
			<div class="col1">
				<fieldset>
					<legend>TestActivities</legend>
					<input type="submit" value="Test" class="button" />
				</fieldset>
			</div>
		</form>	
		<!-- TestActivities10 -->
		<form method="post" action="<%= WIZARD_NAME %>">
			<input type="hidden" name="command" value="TestActivities10"/>
			<input type="hidden" name="<%= Action.PARAMETER_REQUEST_ID %>" value="<%= requestId %>" />
			<input type="hidden" name="<%= Action.PARAMETER_OBJECTXRI %>" value="<%= objectXri %>" />
			<div class="col1">
				<fieldset>
					<legend>TestActivities10</legend>
					<input type="submit" value="Test" class="button" />
				</fieldset>
			</div>
		</form>	
		<!-- TestRegisterCustomer -->
		<form method="post" action="<%= WIZARD_NAME %>">
			<input type="hidden" name="command" value="TestRegisterCustomer"/>
			<input type="hidden" name="<%= Action.PARAMETER_REQUEST_ID %>" value="<%= requestId %>" />
			<input type="hidden" name="<%= Action.PARAMETER_OBJECTXRI %>" value="<%= objectXri %>" />
			<div class="col1">
				<fieldset>
					<legend>TestRegisterCustomer</legend>
					<input type="submit" value="Test" class="button" />
				</fieldset>
			</div>
		</form>	
		<!-- TestRegisterCustomer10 -->
		<form method="post" action="<%= WIZARD_NAME %>">
			<input type="hidden" name="command" value="TestRegisterCustomer10"/>
			<input type="hidden" name="<%= Action.PARAMETER_REQUEST_ID %>" value="<%= requestId %>" />
			<input type="hidden" name="<%= Action.PARAMETER_OBJECTXRI %>" value="<%= objectXri %>" />
			<div class="col1">
				<fieldset>
					<legend>TestRegisterCustomer10</legend>
					<input type="submit" value="Test" class="button" />
				</fieldset>
			</div>
		</form>	
		<!-- TestAll -->
		<form method="post" action="<%= WIZARD_NAME %>">
			<input type="hidden" name="command" value="TestAll"/>
			<input type="hidden" name="<%= Action.PARAMETER_REQUEST_ID %>" value="<%= requestId %>" />
			<input type="hidden" name="<%= Action.PARAMETER_OBJECTXRI %>" value="<%= objectXri %>" />
			<div class="col1">
				<fieldset>
					<legend>TestAll</legend>
					<input type="submit" value="Test" class="button" />
				</fieldset>
			</div>
		</form>	
		<!-- TestAll10 -->
		<form method="post" action="<%= WIZARD_NAME %>">
			<input type="hidden" name="command" value="TestAll10"/>
			<input type="hidden" name="<%= Action.PARAMETER_REQUEST_ID %>" value="<%= requestId %>" />
			<input type="hidden" name="<%= Action.PARAMETER_OBJECTXRI %>" value="<%= objectXri %>" />
			<div class="col1">
				<fieldset>
					<legend>TestAll10</legend>
					<input type="submit" value="Test" class="button" />
				</fieldset>
			</div>
		</form>	
		<!-- TestAllBuy -->
		<form method="post" action="<%= WIZARD_NAME %>">
			<input type="hidden" name="command" value="TestAllBuy"/>
			<input type="hidden" name="<%= Action.PARAMETER_REQUEST_ID %>" value="<%= requestId %>" />
			<input type="hidden" name="<%= Action.PARAMETER_OBJECTXRI %>" value="<%= objectXri %>" />
			<div class="col1">
				<fieldset>
					<legend>TestAllBuy</legend>
					<input type="submit" value="Test" class="button" />
				</fieldset>
			</div>
		</form>	
		<!-- TestAllBuy10 -->
		<form method="post" action="<%= WIZARD_NAME %>">
			<input type="hidden" name="command" value="TestAllBuy10"/>
			<input type="hidden" name="<%= Action.PARAMETER_REQUEST_ID %>" value="<%= requestId %>" />
			<input type="hidden" name="<%= Action.PARAMETER_OBJECTXRI %>" value="<%= objectXri %>" />
			<div class="col1">
				<fieldset>
					<legend>TestAllBuy10</legend>
					<input type="submit" value="Test" class="button" />
				</fieldset>
			</div>
		</form>	
		<!-- TestCreate100Products -->
		<form method="post" action="<%= WIZARD_NAME %>">
			<input type="hidden" name="command" value="TestCreate100Products"/>
			<input type="hidden" name="<%= Action.PARAMETER_REQUEST_ID %>" value="<%= requestId %>" />
			<input type="hidden" name="<%= Action.PARAMETER_OBJECTXRI %>" value="<%= objectXri %>" />
			<div class="col1">
				<fieldset>
					<legend>TestCreate100Products</legend>
					<input type="submit" value="Test" class="button" />
				</fieldset>
			</div>
		</form>	
		<!-- TestCreate100Products10 -->
		<form method="post" action="<%= WIZARD_NAME %>">
			<input type="hidden" name="command" value="TestCreate100Products10"/>
			<input type="hidden" name="<%= Action.PARAMETER_REQUEST_ID %>" value="<%= requestId %>" />
			<input type="hidden" name="<%= Action.PARAMETER_OBJECTXRI %>" value="<%= objectXri %>" />
			<div class="col1">
				<fieldset>
					<legend>TestCreate100Products10</legend>
					<input type="submit" value="Test" class="button" />
				</fieldset>
			</div>
		</form>	
		<fieldset>
			<legend>Status</legend>
			<table>
				<!-- returnStatus -->
				<tr><td><label for="fReturnStatus">Return code:</label></td>
				<td id="fReturnCode"><%= returnStatus == null ? "" : returnStatus.getReturnCode() %></td></tr>
				<!-- returnParams -->
				<tr><td><label for="fReturnParams">Return params:</label></td>
				<td id="fReturnParams"><%= returnStatus == null ? "" : returnStatus.getReturnParams() %></td></tr>
			</table>
		</fieldset>
	</div>
	<form method="post" action="<%= WIZARD_NAME %>">
		<div class="buttons">
			<input type="button" value="Exit" onclick="javascript:location.href='<%= WIZARD_NAME + "?" + Action.PARAMETER_REQUEST_ID + "=" + requestId + "&" + Action.PARAMETER_OBJECTXRI + "=" + URLEncoder.encode(objectXri) + "&command=Cancel" %>';" class="button" />
		</div>
	</form>
</body>
</html>
