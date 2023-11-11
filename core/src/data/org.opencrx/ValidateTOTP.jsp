<%@page contentType="text/html;charset=UTF-8" language="java" pageEncoding="UTF-8" %>
<%@taglib prefix="t" tagdir="/WEB-INF/tags" %>
<%
/*
 * ====================================================================
 * Project:     openCRX/Core, http://www.opencrx.org/
 * Description: ValidateTOTP
 * Owner:       the original authors.
 * ====================================================================
 *
 * This software is published under the BSD license
 * as listed below.
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
java.net.*,
java.util.Enumeration,
java.io.PrintWriter,
org.w3c.spi2.*,
org.openmdx.portal.servlet.*,
org.openmdx.base.naming.*,
org.opencrx.kernel.generic.*
"%>
<%
	final String FORM_NAME = "ValidateTOTP.jsp";
	ApplicationContext app = (ApplicationContext)session.getAttribute(WebKeys.APPLICATION_KEY);
	String appUrl = request.getContextPath() + "/" + WebKeys.SERVLET_NAME;
	if(app == null) {
		// not authenticated --> send to app --> send to login
		session.invalidate();
		response.sendRedirect(appUrl);
	}
	javax.jdo.PersistenceManager pm = app.getNewPmData();
	Path userHomeIdentity = app.getUserHomeIdentityAsPath();
	String providerName = userHomeIdentity.getSegment(2).toString();
	String segmentName = userHomeIdentity.getSegment(4).toString();
	String authKey = org.opencrx.kernel.utils.TOTP.getSessionKey(providerName, segmentName);
	org.opencrx.kernel.home1.jmi1.UserHome userHome = (org.opencrx.kernel.home1.jmi1.UserHome)pm.getObjectById(userHomeIdentity);
	org.opencrx.kernel.home1.jmi1.Media totpInfo = org.opencrx.kernel.backend.UserHomes.getInstance().getMedia(
		userHome,
		org.opencrx.kernel.utils.TOTP.class.getSimpleName()
	);
	if(totpInfo == null) {
		// TOTP not configured --> send to app
		session.invalidate();
		response.sendRedirect(appUrl);
	}
	if(session.getAttribute(authKey) != null) {
		// already authenticated --> send to app
		response.sendRedirect(appUrl);		
	} else {
		String password = request.getParameter("password");
		// validate
		if(password != null && !password.isEmpty()) {
			org.w3c.cci2.BinaryLargeObject content = totpInfo.getContent();
			java.util.Properties totpProperties = new java.util.Properties();
			totpProperties.load(content.getContent());
			String secretKey = totpProperties.getProperty("secretKey");
			if(secretKey != null && !secretKey.isEmpty()) {
				if(org.opencrx.kernel.utils.TOTP.validatePassword(secretKey, password)) {
					// success --> send to app
					session.setAttribute(authKey, Boolean.TRUE);
					response.sendRedirect(appUrl);
				} else {
					// stay on this page and re-enter token 
				}
			} else {
				// invalid TOTP configuration --> logoff --> send to app --> send to login
				session.invalidate();
				response.sendRedirect(appUrl);			
			}
		}
	}
%>
<!DOCTYPE HTML PUBLIC "-//W3C//DTD HTML 4.01 Transitional//EN" "http://www.w3.org/TR/html4/loose.dtd">
<html>
<head>
	<title>Request Password Reset</title>
	<meta http-equiv="Content-Type" content="text/html; charset=utf-8">
	<meta http-equiv="Expires" content="0">
	<meta name="viewport" content="width=320; initial-scale=1.0; maximum-scale=1.0; user-scalable=0;">
	<meta name="apple-touch-fullscreen" content="YES" />
	<!-- Styles -->
	<link rel="stylesheet" href="js/bootstrap/css/bootstrap.min.css">
	<link rel="stylesheet" href="<%= request.getContextPath() %>/_style/ssf.css" >
	<link rel="stylesheet" href="<%= request.getContextPath() %>/_style/n2default.css" >
	<link rel="stylesheet" href="<%= request.getContextPath() %>/_style/colors.css">
	<link rel="stylesheet" href="<%= request.getContextPath() %>/_style/calendar-small.css">
	<link rel='shortcut icon' href='<%= request.getContextPath() %>/images/favicon.ico' />
	<!-- Libraries -->
	<script language="javascript" type="text/javascript" src="<%= request.getContextPath() %>/js/prototype.js"></script>
</head>
<body style="border:0px solid white;">
  <div id="header" style="height:90px;">
    <div id="logoTable">
      <table dir="ltr" id="headerlayout" style="position:relative;">
        <tr id="headRow">
          <td id="head" colspan="2">
            <table id="info">
              <tr>
                <td id="headerCellLeft"><img id="logoLeft" style="cursor:default;" src="<%=request.getContextPath()%>/images/logoLeft.gif" alt="openCRX - limitless relationship management" title="openCRX - limitless relationship management" /></td>
                <td id="headerCellMiddle" style="background-image:url('./images/logoMiddle.gif');background-repeat:repeat-x;width:100%;"></td>
                <td id="headerCellRight"><img id="logoRight" src="<%=request.getContextPath()%>/images/logoRight.gif" alt="" title="" /></td>
              </tr>
            </table>
          </td>
        </tr>
      </table>
    </div>
  </div>
  <div class="container">
  	<div class="row">
  		<div class="col-sm-12">
		    <form role="form" class="form-signin" style="max-width:400px;margin:0 auto;" method="POST" action="<%= FORM_NAME %>" accept-charset="UTF-8">
				<input type="text" name="password" id="password" autofocus="" placeholder="TOTP <%= userHomeIdentity.getLastSegment() + "@" + providerName + "/" + segmentName %>" class="form-control" />
				<br />
				<button type="submit" class="btn btn-lg btn-primary btn-block">OK</button>
				<br />
  			</form>
		</div>
	</div>
  </div>
</body>
</html>
