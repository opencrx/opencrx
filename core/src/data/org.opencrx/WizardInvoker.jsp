<%@  page contentType= "text/html;charset=UTF-8" language="java" pageEncoding= "UTF-8" %><%
/*
 * ====================================================================
 * Project:		openCRX/Core, http://www.opencrx.org/
 * Description: WizardInvoker
 * Owner:		CRIXP Corp., Switzerland, http://www.crixp.com
 * ====================================================================
 *
 * This software is published under the BSD license
 * as listed below.
 *
 * Copyright (c) 2010-2014, CRIXP Corp., Switzerland
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
%><%@ page buffer="32kb" session="true" import="
java.util.*,
java.io.*,
java.text.*,
org.openmdx.base.exception.*,
java.io.PrintWriter,
java.net.HttpURLConnection,
java.net.MalformedURLException,
java.net.URL
" %>
<%
	/**
	 *	The WizardInvoker is invoked with the following URL parameters:
	 *	- wizard: path of the wizard JSP
	 *	- provider: provider name
	 *	- segment: segment name
	 *	- xri: target object xri
	 *	- user: user name
	 *	- password: password
	 *  - para_0, para_1, ... para_n: additional parameters to be passed to the wizard (optional)
	 *	Example:
	 *	http://localhost:8080/opencrx-core-CRX/WizardInvoker.jsp?wizard=/wizards/en_US/UploadMedia.jsp&provider=CRX&segment=Standard&xri=xri://@openmdx*org.opencrx.kernel.home1/provider/CRX/segment/Standard&user=wfro&password=.
	 *
	 *	WizardInvoker performs the following operations:
	 *	- Creates a session and authenticates with the specified user and password
	 *	- Invokes ObjectInspectorServlet to initialize the application context
	 *	- Invokes the specified wizard with provider, segment, xri, and - optionally - para_0
	 *    through para_n as parameter, e.g. "&para_0=auto=true&para_1=timeout=10"
	 */
	request.setCharacterEncoding("UTF-8");
%>
<!DOCTYPE HTML PUBLIC "-//W3C//DTD HTML 4.01 Transitional//EN" "http://www.w3.org/TR/html4/loose.dtd">
<html>
<head>
	<title>WizardInvoker</title>
	<meta http-equiv="Content-Type" content="text/html; charset=utf-8">
</head>
<body>
<%
	final boolean LOG_TRACE = true;
	String objectInspectorServlet = "/" + org.openmdx.portal.servlet.WebKeys.SERVLET_NAME;
	String urlBase = (request.getRequestURL().toString()).substring(0, (request.getRequestURL().toString()).indexOf(request.getServletPath().toString()));

	if (LOG_TRACE) {
		System.out.println(">---------" + request.getServletPath() + "----------(invocationBegin)");
	}
	try {
		String providerName = request.getParameter("provider");
		String segmentName = request.getParameter("segment");
		String xri = request.getParameter("xri");
		String wizard = request.getParameter("wizard");
		if ((providerName == null) || (providerName.length() == 0) || (segmentName == null) || (segmentName.length() == 0)) {
			if (LOG_TRACE) {
				System.out.println(new java.util.Date().toString() + ": providerName [=" + providerName + "] or segmentName [=" + segmentName + "] missing");
			}
			return;
		}
		wizard += "?provider=" + providerName + "&segment=" + segmentName + "&xri=" + xri;
		// get optional/additional parameters
		int paraIdx = 0;
		while (request.getParameter("para_" + paraIdx) != null) {
			wizard += "&" + request.getParameter("para_" + paraIdx);
			paraIdx += 1;
		}
		String userId = request.getParameter("user");
		String passwd = request.getParameter("password");
		System.out.println("user=" + userId + "; password=???");
		URL url = new URL(urlBase + wizard);
		URL urlObjectInspectorServlet = new URL(urlBase + objectInspectorServlet);

		// ObjectInspectorServlet to get session Cookie
		HttpURLConnection connection = (HttpURLConnection)urlObjectInspectorServlet.openConnection();
		int rc = connection.getResponseCode();
		if(LOG_TRACE) {
			System.out.println(new java.util.Date().toString() + ": " + rc + " - (" + urlObjectInspectorServlet + ")");
		}
		String cookie = connection.getHeaderField("Set-Cookie");
		if (LOG_TRACE) {
			System.out.println(new java.util.Date().toString() + ": Cookie: " + cookie);
		}
		connection.disconnect();

		// j_security_check
		String urlJSecurityCheck = urlBase + "/j_security_check" + "?j_username=" + userId + "&j_password=" + passwd;
		connection = (HttpURLConnection) new URL(urlJSecurityCheck).openConnection();
		connection.setRequestMethod("POST");
		connection.setRequestProperty("Cookie", cookie);
		connection.setInstanceFollowRedirects(false);
		rc = connection.getResponseCode();
		if(LOG_TRACE) {
			System.out.println(new java.util.Date().toString() + ": " + rc + " - (" + urlJSecurityCheck + ")");
		}
		if(LOG_TRACE) {
			System.out.println(new java.util.Date().toString() + ": Cookie: " + cookie);
		}
		connection.disconnect();

		// ./ObjectInspectorServlet to initialize session
		connection = (HttpURLConnection)urlObjectInspectorServlet.openConnection();
		connection.setRequestProperty("Cookie", cookie);
		rc = connection.getResponseCode();
		if(LOG_TRACE) {
			System.out.println(new java.util.Date().toString() + ": Header fields: " + connection.getHeaderFields());
		}
		if (LOG_TRACE) {
			System.out.println(new java.util.Date().toString() + ": " + rc + " - (" + urlObjectInspectorServlet + ")");
		}
		cookie = connection.getHeaderField("Set-Cookie");
		connection.disconnect();

		// Invoke wizard
		connection = (HttpURLConnection)url.openConnection();
		connection.setRequestProperty("Cookie", cookie);
		rc = connection.getResponseCode();
		if(LOG_TRACE) {
			System.out.println(new java.util.Date().toString() + ": " + rc + " - (" + url + ")");
			System.out.println("<---------" + request.getServletPath() + "----------(invocationEnd)");
		}
		connection.disconnect();
	}
	catch (Exception ex) {
		System.out.println("Exception WizardInvoker");
		new ServiceException(ex).log();
	}
%>
</body>
</html>
