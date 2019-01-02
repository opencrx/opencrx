<%@  page contentType="text/html;charset=UTF-8" language="java" pageEncoding="UTF-8" %><%
/*
 * ====================================================================
 * Project:     opencrx, http://www.opencrx.org/
 * Name:        Activity-close.jsp
 * Description: close activities
 * Owner:       CRIXP Corp., Switzerland, http://www.crixp.com
 * ====================================================================
 *
 * This software is published under the BSD license
 * as listed below.
 *
 * Copyright (c) 2012, CRIXP Corp., Switzerland
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
java.math.*,
java.net.*,
java.sql.*,
javax.naming.Context,
javax.naming.InitialContext,
org.openmdx.kernel.id.cci.*,
org.openmdx.kernel.id.*,
org.openmdx.base.accessor.jmi.cci.*,
org.openmdx.base.exception.*,
org.openmdx.portal.servlet.*,
org.openmdx.portal.servlet.attribute.*,
org.openmdx.portal.servlet.component.*,
org.openmdx.portal.servlet.control.*,
org.openmdx.portal.servlet.wizards.*,
org.openmdx.base.naming.*,
org.openmdx.base.query.*,
org.openmdx.kernel.log.*
" %><%
	request.setCharacterEncoding("UTF-8");
	ApplicationContext app = (ApplicationContext)session.getValue(WebKeys.APPLICATION_KEY);
	ViewsCache viewsCache = (ViewsCache)session.getValue(WebKeys.VIEW_CACHE_KEY_SHOW);
	String requestId =  request.getParameter(Action.PARAMETER_REQUEST_ID);
	String requestIdParam = Action.PARAMETER_REQUEST_ID + "=" + requestId;
	String objectXri = request.getParameter("xri");
	if(app == null || objectXri == null || viewsCache.getView(requestId) == null) {
	    response.sendRedirect(
	       request.getContextPath() + "/" + WebKeys.SERVLET_NAME
	    );
	    return;
	}
	javax.jdo.PersistenceManager pm = app.getNewPmData();
	Texts_1_0 texts = app.getTexts();

	final String FORM_ACTION = "Activity-close.jsp";
	String command = request.getParameter("Command");
	if(command == null) command = "";
	boolean actionOK = "OK".equals(command);
%>
<!DOCTYPE HTML PUBLIC "-//W3C//DTD HTML 4.01 Transitional//EN" "http://www.w3.org/TR/html4/loose.dtd">

<html>

<head>
  <title>Close Activities</title>
  <meta http-equiv="Content-Type" content="text/html; charset=utf-8">
  <meta http-equiv="Content-Type" content="text/html; charset=utf-8">
  <link rel="stylesheet" href="../../js/bootstrap/css/bootstrap.min.css">	
  <link rel="stylesheet" href="../../_style/colors.css">
  <link rel="stylesheet" href="../../_style/n2default.css">
  <link rel="stylesheet" href="../../_style/ssf.css">
  <script language="javascript" type="text/javascript" src="../../js/prototype.js"></script>
  <link rel='shortcut icon' href='../../images/favicon.ico' />
</head>

<body>
  <div id="container">
    <div id="wrap">
      <div id="eheader" style="height:90px;width:100%;">
	      <div id="logoTable">
	        <table id="headerlayout">
	          <tr id="headRow">
	            <td id="head" colspan="2">
	              <table id="info">
	                <tr>
	                  <td id="headerCellLeft"><img id="logoLeft" src="../../images/logoLeft.gif" alt="openCRX" title="" /></td>
	                  <td id="headerCellSpacerLeft"></td>
	                  <td id="headerCellMiddle">&nbsp;</td>
	                  <td id="headerCellRight"><img id="logoRight" src="../../images/logoRight.gif" alt="" title="" /></td>
	                </tr>
	              </table>
	            </td>
	          </tr>
	        </table>
	      </div>
	  </div>

      <div id="content-wrap">
        <div id="econtent" style="padding:100px 0.5em 0px 0.5em;">
          <form name="ConnectionHelper" accept-charset="UTF-8" method="POST" action="<%= "../.." + request.getServletPath() %>">
			<input type="hidden" name="<%= Action.PARAMETER_OBJECTXRI %>" value="<%= objectXri %>" />
			<input type="hidden" name="<%= Action.PARAMETER_REQUEST_ID %>" value="<%= requestId %>" />
			<input type="Hidden" id="Command" name="Command" value="" />
			<input type="checkbox" style="display:none;" id="isFirstCall" name="isFirstCall" checked />


				<h2>Closing Activities</h2>
				<pre>
<%
					long updateCount = 0;
					try {
						Path objectPath = new Path(objectXri);
						RefObject_1_0 obj = (RefObject_1_0)pm.getObjectById(objectPath);
				
				        org.opencrx.kernel.activity1.cci2.ActivityQuery activityFilter = (org.opencrx.kernel.activity1.cci2.ActivityQuery)pm.newQuery(org.opencrx.kernel.activity1.jmi1.Activity.class);
				        activityFilter.orderByActivityNumber().ascending();
				        activityFilter.forAllDisabled().isFalse();
				        
				        List<String> activityXris = new ArrayList<String>();
				        for (Iterator a =  ((org.opencrx.kernel.activity1.jmi1.ActivityFilterGlobal)obj).getFilteredActivity(activityFilter).iterator(); a.hasNext();) {
				        	try {
				        		activityXris.add(((org.opencrx.kernel.activity1.jmi1.Activity)a.next()).refMofId());
				        	} catch (Exception e) {
				        		new ServiceException(e).log();
				        	}
				        }
				        
				        if (actionOK) {
				
							org.opencrx.kernel.activity1.jmi1.Activity activity = null;
							
							for (Iterator a = activityXris.iterator(); a.hasNext();) {
								String activityHref = "";
								try {
									activity = (org.opencrx.kernel.activity1.jmi1.Activity)pm.getObjectById(new Path((String)a.next()));
									Action action = new ObjectReference(
										activity,
										app
									).getSelectObjectAction();
									activityHref = action.getEncodedHRef();
									pm.currentTransaction().begin();
									activity.setActivityState((short)20); // close
									activity.setPercentComplete((short)100);
									pm.currentTransaction().commit();
									updateCount++;
%>OK    : <a href="../../<%= activityHref %>" target="_blank"><%= (new ObjectReference(activity, app)).getTitle() %></a>
<%
								} catch (Exception e) {
%>FAILED: <a href="../../<%= activityHref %>" target="_blank"><%= (new ObjectReference(activity, app)).getTitle() %></a>
<%
									new ServiceException(e).log();
								    try {
								      pm.currentTransaction().rollback();
								    } catch (Exception ex) {}
								}
							}
%>
Done - closed <%= updateCount %> <%= updateCount == 1 ? "Activity" : "Activities" %>!
<%
				        } else {
%>
Close <%= activityXris.size() %> <%= activityXris.size() == 1 ? "Activity" : "Activities" %>?
<%
				        }
					} catch (Exception e) {
						new ServiceException(e).log();
					}
					finally {
					  if(pm != null) {
					  	pm.close();
					  }
					}	
					System.out.println("closed a total of " + updateCount + " activities");
%>
				</pre>
			    <input type="submit" id="Reload.button" name="Reload" class="<%= CssClass.btn.toString() %> <%= CssClass.btnDefault.toString() %>" tabindex="9000" <%= actionOK ? "" : "style='display:none;'" %> value="<%= app.getTexts().getReloadText() %>" onclick="javascript:$('Command').value=this.name;" />
			    <input type="submit" id="OK.button"     name="OK"     class="<%= CssClass.btn.toString() %> <%= CssClass.btnDefault.toString() %>" tabindex="9010" <%= actionOK ? "style='display:none;'" : "" %> value="<%= app.getTexts().getOkTitle()    %>" onclick="javascript:$('Command').value=this.name;" />
			    <input type="submit" id="Cancel.button" name="Cancel" class="<%= CssClass.btn.toString() %> <%= CssClass.btnDefault.toString() %>" tabindex="9020" value="<%= app.getTexts().getCancelTitle() %>"  onClick="javascript:window.close();" />
			    <br>
          </form>
        </div> <!-- content -->
      </div> <!-- content-wrap -->
     </div> <!-- wrap -->
  </div> <!-- container -->
</body>
</html>
