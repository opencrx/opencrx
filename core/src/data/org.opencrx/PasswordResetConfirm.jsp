<%@page contentType="text/html;charset=UTF-8" language="java" pageEncoding="UTF-8" %>
<%@taglib prefix="t" tagdir="/WEB-INF/tags" %>
<%
/*
 * ====================================================================
 * Project:     openCRX/Core, http://www.opencrx.org/
 * Description: PasswordResetConfirm
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
java.net.*,
java.util.Enumeration,
java.io.PrintWriter,
org.w3c.spi2.*,
org.openmdx.portal.servlet.*,
org.openmdx.base.naming.*
"%>
<%
	Boolean success = null;
	String resetToken = request.getParameter("t");
	String providerName = request.getParameter("p");
	String segmentName = request.getParameter("s");
	String id = request.getParameter("id");
	String password1 = request.getParameter("password1");
	String password2 = request.getParameter("password2");
	if(
		resetToken != null && !resetToken.isEmpty() &&
		providerName != null && !providerName.isEmpty() &&
		segmentName != null && !segmentName.isEmpty() &&
		id != null && !id.isEmpty() &&
		password1 != null && !password1.isEmpty() &&
		password2 != null && !password2.isEmpty()
	) {
		javax.jdo.PersistenceManagerFactory pmf = org.opencrx.kernel.utils.Utils.getPersistenceManagerFactory();
		javax.jdo.PersistenceManager pm = pmf.getPersistenceManager(id, null);
		try {
			org.opencrx.kernel.home1.jmi1.UserHome userHome = (org.opencrx.kernel.home1.jmi1.UserHome)pm.getObjectById(
				new Path("xri://@openmdx*org.opencrx.kernel.home1").getDescendant("provider", providerName, "segment", segmentName, "userHome", id)
			);
			pm.currentTransaction().begin();
			org.opencrx.kernel.home1.jmi1.ChangePasswordParams params = Structures.create(
				org.opencrx.kernel.home1.jmi1.ChangePasswordParams.class, 
	           	Datatypes.member(org.opencrx.kernel.home1.jmi1.ChangePasswordParams.Member.oldPassword, org.opencrx.kernel.backend.UserHomes.RESET_PASSWORD_PREFIX + resetToken),
	           	Datatypes.member(org.opencrx.kernel.home1.jmi1.ChangePasswordParams.Member.newPassword, password1),
	           	Datatypes.member(org.opencrx.kernel.home1.jmi1.ChangePasswordParams.Member.newPasswordVerification, password2)           	
	        );
			org.opencrx.kernel.home1.jmi1.ChangePasswordResult result = userHome.changePassword(params);
			pm.currentTransaction().commit();
			success = result.getStatus() == 0;
		} catch(Exception e) {
			try {
				pm.currentTransaction().rollback();
			} catch(Exception ignore) {}
			success = false;
		}
	}
%>
<!DOCTYPE HTML PUBLIC "-//W3C//DTD HTML 4.01 Transitional//EN" "http://www.w3.org/TR/html4/loose.dtd">
<html>
<head>
	<title>Confirm Password Reset</title>
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
<%
			if(Boolean.TRUE.equals(success)) {
%>
				<h2>Password successfully changed for <%= id %>@<%= providerName + "/" + segmentName %></h2>
<%
			} else if(Boolean.FALSE.equals(success)) {
%>
				<h2>Unable to reset password</h2>
<%
			} else if(
				resetToken != null && !resetToken.isEmpty() &&
				providerName != null && !providerName.isEmpty() &&
				segmentName != null && !segmentName.isEmpty() &&
				id != null && !id.isEmpty()
			) {
%>
			    <form role="form" class="form-signin" style="max-width:400px;margin:0 auto;" method="POST" action="PasswordResetConfirm.jsp" accept-charset="UTF-8">
      				<h2 class="form-signin-heading">Reset password for <%= id %>@<%= providerName + "/" + segmentName %></h2>					
			    	<input type="hidden" name="t" value="<%= resetToken %>" />
			    	<input type="hidden" name="p" value="<%= providerName %>" />
			    	<input type="hidden" name="s" value="<%= segmentName %>" />
			    	<input type="hidden" name="id" value="<%= id %>" />
					<input type="password" name="password1" autofocus="" placeholder="Password" class="form-control" />
					<input type="password" name="password2" placeholder="Password (verify)" class="form-control" />
					<br />
					<button type="submit" class="btn btn-lg btn-primary btn-block">OK</button>
					<br />
      				<%@ include file="password-reset-confirm-note.html" %>					
			    </form>
<%				
			} else {
%>
				<h2>Invalid password confirm request</h2>
<%
			}
%>
		</div>
	</div>
  	<div class="row">
  		<div class="col-sm-12">
  			<a href="./Login.jsp">Go to login page</a>
  		</div>
  	</div>	
  </div>
</body>
</html>
