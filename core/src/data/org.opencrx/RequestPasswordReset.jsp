<%@page contentType="text/html;charset=UTF-8" language="java" pageEncoding="UTF-8" %>
<%@taglib prefix="t" tagdir="/WEB-INF/tags" %>
<%
/*
 * ====================================================================
 * Project:     openCRX/Core, http://www.opencrx.org/
 * Description: RequestPasswordReset
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
org.openmdx.base.naming.*,
org.opencrx.kernel.generic.*,
org.opencrx.kernel.backend.*
"%>
<%
	Boolean success = null;
	String id = request.getParameter("id");
	if(
		id != null && !id.isEmpty()
	) {
		final Path LOGIN_REALM_IDENTITY = new Path("xri://@openmdx*org.openmdx.security.realm1/provider/CRX/segment/Root/realm/Default");

		String principalName = null;
		String providerName = null;
		String segmentName = null;
		
		int pos1 = id.indexOf("@");
		int pos2 = id.indexOf("/");
		if(pos1 > 0 && pos2 > pos1) {	
			// Format 1: principal@provider/segment
			principalName = id.substring(0, pos1);
			providerName = id.substring(pos1 + 1, pos2);
			segmentName = id.substring(pos2 + 1);
		} else if(pos1 > 0) {
			// Format 2: mail@domain
			javax.jdo.PersistenceManagerFactory pmf = org.opencrx.kernel.utils.Utils.getPersistenceManagerFactory();
			javax.jdo.PersistenceManager pmRoot = pmf.getPersistenceManager(
				SecurityKeys.ROOT_PRINCIPAL, 
				null
			);
	        org.openmdx.security.realm1.jmi1.Segment realmSegment =
	            (org.openmdx.security.realm1.jmi1.Segment)pmRoot.getObjectById(LOGIN_REALM_IDENTITY.getParent().getParent());
	        int count = 0;
	        for(org.openmdx.security.realm1.jmi1.Realm realm: realmSegment.<org.openmdx.security.realm1.jmi1.Realm>getRealm()) {
	            if(!realm.refGetPath().equals(LOGIN_REALM_IDENTITY) && !"Root".equals(realm.refGetPath().getLastSegment().toClassicRepresentation())) {
           			String currentProviderName = realm.refGetPath().getSegment(2).toClassicRepresentation();
           			String currentSegmentName = realm.refGetPath().getLastSegment().toClassicRepresentation();
	    			javax.jdo.PersistenceManager pm = pmf.getPersistenceManager(
    					SecurityKeys.ADMIN_PRINCIPAL + SecurityKeys.ID_SEPARATOR + currentSegmentName, 
    					null
    				);
	            	org.opencrx.kernel.home1.jmi1.Segment userHomeSegment = UserHomes.getInstance().getUserHomeSegment(pm, currentProviderName, currentSegmentName);
	            	org.opencrx.kernel.home1.cci2.EMailAccountQuery emailAccountQuery = (org.opencrx.kernel.home1.cci2.EMailAccountQuery)org.openmdx.base.persistence.cci.PersistenceHelper.newQuery(
			    		pm.getExtent(org.opencrx.kernel.home1.jmi1.EMailAccount.class),
			    		userHomeSegment.refGetPath().getDescendant("userHome", ":*", "eMailAccount", ":*")
			    	);
	            	emailAccountQuery.thereExistsIsActive().isTrue();
	            	emailAccountQuery.name().equalTo(id);
					List<org.opencrx.kernel.home1.jmi1.EMailAccount> emailAccounts = userHomeSegment.getExtent(emailAccountQuery);
					if(emailAccounts.size() == 1) {
						org.opencrx.kernel.home1.jmi1.EMailAccount emailAccount = emailAccounts.iterator().next();
						principalName = emailAccount.refGetPath().getParent().getParent().getLastSegment().toClassicRepresentation();
						providerName = currentProviderName;
						segmentName = currentSegmentName;
						count++;
					}
					pm.close();
	            }
	        }
	        // id is not unique --> no success
	        if(count > 1) {
	        	principalName = null;
	        }
	        pmRoot.close();
		} else {
			// Format 3: principal
			javax.jdo.PersistenceManagerFactory pmf = org.opencrx.kernel.utils.Utils.getPersistenceManagerFactory();
			javax.jdo.PersistenceManager pmRoot = pmf.getPersistenceManager(
				SecurityKeys.ROOT_PRINCIPAL, 
				null
			);			
	        org.openmdx.security.realm1.jmi1.Segment realmSegment =
	            (org.openmdx.security.realm1.jmi1.Segment)pmRoot.getObjectById(LOGIN_REALM_IDENTITY.getParent().getParent());
	        int count = 0;
	        for(org.openmdx.security.realm1.jmi1.Realm realm: realmSegment.<org.openmdx.security.realm1.jmi1.Realm>getRealm()) {
	            if(!realm.refGetPath().equals(LOGIN_REALM_IDENTITY) && !"Root".equals(realm.refGetPath().getLastSegment().toClassicRepresentation())) {
	           		org.openmdx.security.realm1.jmi1.Principal principal = SecureObject.getInstance().findPrincipal(id, realm);
	           		if(principal != null && !Boolean.TRUE.equals(principal.isDisabled())) {
	           			principalName = id;
	           			providerName = realm.refGetPath().getSegment(2).toClassicRepresentation();
	           			segmentName = realm.refGetPath().getLastSegment().toClassicRepresentation();
	           			count++;
	           		}
	            }
			}
	        // id is not unique --> no success
	        if(count > 1) {
	        	principalName = null;
	        }
	        pmRoot.close();
	    }
		if(principalName != null && providerName != null && segmentName != null) {
			javax.jdo.PersistenceManagerFactory pmf = org.opencrx.kernel.utils.Utils.getPersistenceManagerFactory();
			javax.jdo.PersistenceManager pm = pmf.getPersistenceManager(
				SecurityKeys.ADMIN_PRINCIPAL + SecurityKeys.ID_SEPARATOR + segmentName, 
				null
			);
			try {
				org.opencrx.kernel.home1.jmi1.UserHome userHome = (org.opencrx.kernel.home1.jmi1.UserHome)pm.getObjectById(
					new Path("xri://@openmdx*org.opencrx.kernel.home1").getDescendant("provider", providerName, "segment", segmentName, "userHome", principalName)
				);
				pm.currentTransaction().begin();
				userHome.requestPasswordReset();
				pm.currentTransaction().commit();
				success = true;
			} catch(Exception e) {
				try {
					pm.currentTransaction().rollback();
				} catch(Exception ignore) {}
				success = false;
			}
		} else {
			success = false;
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
<%
			if(Boolean.TRUE.equals(success)) {
%>
				<h2>Password reset request successful for <%= id %></h2>
				<p>
				You should receive a notification e-mail within the next minutes.
				<p>
<%
			} else if(Boolean.FALSE.equals(success)) {
%>
				<h2>Unable to request password reset</h2>
<%
			} else {
%>
			    <form role="form" class="form-signin" style="max-width:400px;margin:0 auto;" method="POST" action="RequestPasswordReset.jsp" accept-charset="UTF-8">
      				<h2 class="form-signin-heading">Please enter your username, e-mail address or ID</h2>					
					<input type="text" name="id" id="id" autofocus="" placeholder="ID (e.g. guest@CRX/Standard)" class="form-control" />
					<br />
					<button type="submit" class="btn btn-lg btn-primary btn-block">OK</button>
					<br />
      				<%@ include file="request-password-reset-note.html" %>
   			    </form>
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
