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
 * Copyright (c) 2010-2020, CRIXP Corp., Switzerland
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
java.util.logging.*,
java.io.PrintWriter,
java.net.HttpURLConnection,
java.net.MalformedURLException,
java.net.URL,
org.openmdx.kernel.log.*,
org.openmdx.base.exception.*
" %>
<%!

	public static String doService(
		URL url,
		String method,
		String cookie,
		Boolean instanceFollowRedirects,
		ByteArrayOutputStream response,
		Level logLevel
	) throws IOException {
		HttpURLConnection connection = (HttpURLConnection)url.openConnection();
		if(cookie != null) {
			connection.setRequestProperty("Cookie", cookie);
		}
		if(method != null) {
			connection.setRequestMethod(method);
			if("POST".equals(method)) {
				connection.setDoOutput(true);
			}
		}
		if(instanceFollowRedirects != null) {
			connection.setInstanceFollowRedirects(instanceFollowRedirects);
		}
		int status = connection.getResponseCode();
		SysLog.log(logLevel, "status: {0}; url: {1}", status, url);
		String setCookie = connection.getHeaderField("Set-Cookie");
		if(setCookie != null) {
			SysLog.log(logLevel, "Set-Cookie: {0}", setCookie);
		}
		{
			if(response == null) {
				response = new ByteArrayOutputStream();
			}
			org.w3c.cci2.BinaryLargeObjects.streamCopy(connection.getInputStream(), 0L, response);
			response.close();
		}
		return setCookie == null ? cookie : setCookie;
	}
%>
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
	 *	http://localhost:8080/opencrx-core-CRX/WizardInvoker.jsp?wizard=/wizards/PrintConsole/index.jsp&provider=CRX&segment=Standard&xri=xri://@openmdx*org.opencrx.kernel.home1/provider/CRX/segment/Standard&message=hello&user=guest&password=guest
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
	String objectInspectorServlet = "/" + org.openmdx.portal.servlet.WebKeys.SERVLET_NAME;
	String urlBase = (request.getRequestURL().toString()).substring(0, (request.getRequestURL().toString()).indexOf(request.getServletPath().toString()));
	try {
		String providerName = request.getParameter("provider");
		String segmentName = request.getParameter("segment");
		String xri = request.getParameter("xri");
		String wizard = request.getParameter("wizard");
		String userId = request.getParameter("user");
		String passwd = request.getParameter("password");		
		Level logLevel = Level.INFO;
		try {
			logLevel = Level.parse(request.getParameter("logLevel"));
		} catch(Exception ignore) {}
		String info = " >********** " + request.getServletPath() + " [" + wizard + "] **********< invocation";
		SysLog.log(logLevel, info + " BEGIN");
		if((providerName == null) || (providerName.length() == 0) || (segmentName == null) || (segmentName.length() == 0)) {
			SysLog.log(logLevel, "providerName: {0} or segmentName: {1} missing", providerName, segmentName);
			return;
		}
		wizard += "?provider=" + providerName + "&segment=" + segmentName + "&xri=" + xri;
		// Get parameters
		int paraIdx = 0;
		while(request.getParameter("para_" + paraIdx) != null) {
			wizard += "&" + request.getParameter("para_" + paraIdx);
			paraIdx += 1;
		}
		URL url = new URL(urlBase + wizard);
		URL urlObjectInspectorServlet = new URL(urlBase + objectInspectorServlet);
		URL urlJSecurityCheck = new URL(urlBase + "/j_security_check?j_username=" + userId + "&j_password=" + passwd);
		String cookie = null;
		// Get a valid jsessionid
		cookie = doService(urlObjectInspectorServlet, null, null, null, null, logLevel);
		// j_security_check
		cookie = doService(urlJSecurityCheck, "POST", cookie, false, null, logLevel);
		// Initialize inspector
		cookie = doService(urlObjectInspectorServlet, null, cookie, null, null, logLevel);
		// Invoke wizard
		{
			ByteArrayOutputStream res = new ByteArrayOutputStream();
			cookie = doService(url, "GET", cookie, null, res, logLevel);
			SysLog.log(Level.FINE, "response: {0}", res.toString("UTF-8"));		
			out.print(res.toString("UTF-8"));
			SysLog.log(logLevel, info + " END");		
		}
	} catch (Exception ex) {
		SysLog.log(Level.WARNING, "Exception WizardInvoker", ex);
	}
%>
</body>
</html>
