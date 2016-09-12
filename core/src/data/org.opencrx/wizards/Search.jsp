<%@page contentType="text/html;charset=UTF-8" language="java" pageEncoding="UTF-8" %>
<%
/*
 * ====================================================================
 * Project:     openCRX/Core, http://www.opencrx.org/
 * Description: Search.jsp
 * Owner:       CRIXP AG, Switzerland, http://www.crixp.com
 * ====================================================================
 *
 * This software is published under the BSD license
 * as listed below.
 *
 * Copyright (c) 2007-2013, CRIXP Corp., Switzerland
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
java.net.*,
java.util.*,
java.io.*,
java.text.*,
org.openmdx.base.naming.*,
org.openmdx.base.query.*,
org.openmdx.base.accessor.jmi.cci.*,
org.openmdx.base.exception.*,
org.openmdx.portal.servlet.*,
org.openmdx.portal.servlet.attribute.*,
org.openmdx.portal.servlet.component.*,
org.openmdx.portal.servlet.control.*,
org.openmdx.portal.servlet.wizards.*
" %>
<%
	request.setCharacterEncoding("UTF-8");
	ApplicationContext app = (ApplicationContext)session.getValue("ObjectInspectorServlet.ApplicationContext");
	ViewsCache viewsCache = (ViewsCache)session.getValue(WebKeys.VIEW_CACHE_KEY_SHOW);
	String requestId =  request.getParameter(Action.PARAMETER_REQUEST_ID);
	if(app == null) {
		response.sendRedirect(
			request.getContextPath() + "/" + WebKeys.SERVLET_NAME
		);
		return;
	}
	javax.jdo.PersistenceManager pm = app.getNewPmData();
	Texts_1_0 texts = app.getTexts();
%>
<!DOCTYPE HTML PUBLIC "-//W3C//DTD HTML 4.01 Transitional//EN" "http://www.w3.org/TR/html4/loose.dtd">
<html dir="<%= texts.getDir() %>">
<head>
  <title><%= app.getApplicationName() + " - Search" %></title>
  <meta name="label" content="Search">
  <meta name="toolTip" content="Search">
  <meta name="targetType" content="_blank">
  <meta name="order" content="9999">
  <meta http-equiv="Content-Type" content="text/html; charset=utf-8">
  <link href="../../_style/colors.css" rel="stylesheet" type="text/css">
  <link href="../../_style/n2default.css" rel="stylesheet" type="text/css">
  <link rel='shortcut icon' href='../../images/favicon.ico' />
</head>
<body>
  <table><tr><td>
<%
	try {
		String searchExpression = request.getParameter("searchExpression");
		// Lookup object by its XRI
		if(searchExpression.startsWith("xri:")) {
			try {
				RefObject_1_0 object = (RefObject_1_0)pm.getObjectById(new Path((searchExpression.trim()).replace("+", "%2B")));
				Action nextAction = new ObjectReference(object, app).getSelectObjectAction();
				response.sendRedirect(
					request.getContextPath() + "/" + nextAction.getEncodedHRef()
				);
				return;
			} catch(Exception e) {}
		}
		// Search by expression
		else {
			// Get home1 package
			org.opencrx.kernel.home1.jmi1.UserHome userHome = null;
			try {
				userHome = (org.opencrx.kernel.home1.jmi1.UserHome)pm.getObjectById(app.getUserHomeIdentityAsPath());
			} catch(Exception e) {
			  // no or broken userhome --> would not be able to build ObjectFinder
    		response.sendRedirect(
    			request.getContextPath() + "/" + WebKeys.SERVLET_NAME
    	  );
				return;
			}
			org.opencrx.kernel.home1.jmi1.SearchResult searchResult = null;
			try {
				pm.currentTransaction().begin();
				org.opencrx.kernel.home1.jmi1.SearchBasicParams params = org.w3c.spi2.Structures.create(
						org.opencrx.kernel.home1.jmi1.SearchBasicParams.class, 
					org.w3c.spi2.Datatypes.member(org.opencrx.kernel.home1.jmi1.SearchBasicParams.Member.searchExpression, searchExpression)
				);
				searchResult = userHome.searchBasic(params);
				pm.currentTransaction().commit();
			} catch(Exception e) {
				System.out.println(e.getCause());
				try {
					pm.currentTransaction().rollback();
				} catch(Exception e0) {}
			}
			if((searchResult != null) && (searchResult.getObjectFinder() != null)) {
				org.opencrx.kernel.home1.jmi1.ObjectFinder objectFinder =
					(org.opencrx.kernel.home1.jmi1.ObjectFinder)pm.getObjectById(new Path(searchResult.getObjectFinder().refMofId()));
				Action nextAction = new ObjectReference(objectFinder, app).getSelectObjectAction();
				response.sendRedirect(
					request.getContextPath() + "/" + nextAction.getEncodedHRef()
				);
				return;
			}
		}
		response.sendRedirect(
			request.getContextPath() + "/" + WebKeys.SERVLET_NAME
		);
	}
  catch (Exception e) {
		new ServiceException(e).log();
  } finally {
	  if(pm != null) {
		  pm.close();
	  }
  }
%>
  </td></tr></table>
</body>
</html>
