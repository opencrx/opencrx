<%@page contentType="text/html;charset=UTF-8" language="java" pageEncoding="UTF-8" %>
<%@taglib prefix="t" tagdir="/WEB-INF/tags" %>
<%
/*
 * ====================================================================
 * Project:     opencrx, http://www.opencrx.org/
 * Description: ConnectionHelper: Generate Adapter URLs
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
%>
<%@page session="true" import="
java.util.*,
java.util.zip.*,
java.io.*,
java.text.*,
java.math.*,
java.net.*,
java.sql.*,
org.opencrx.portal.wizard.*,
org.opencrx.portal.wizard.ConnectionHelperController.SelectorType,
org.opencrx.application.utils.*,
org.openmdx.base.accessor.jmi.cci.*,
org.openmdx.base.exception.*,
org.openmdx.kernel.id.*,
org.openmdx.portal.servlet.*,
org.openmdx.portal.servlet.attribute.*,
org.openmdx.portal.servlet.component.*,
org.openmdx.portal.servlet.control.*,
org.openmdx.portal.servlet.wizards.*,
org.openmdx.base.naming.*,
org.openmdx.base.query.*,
org.openmdx.kernel.log.*
" %>
<%
	final String WIZARD_NAME = "ConnectionHelper.jsp";
	ConnectionHelperController wc = new ConnectionHelperController();
%>
	<t:wizardHandleCommand controller='<%= wc %>' defaultCommand='Reload' />
<%
	if(response.getStatus() != HttpServletResponse.SC_OK) {
		wc.close();		
		return;
	}
	ApplicationContext app = wc.getApp();
	javax.jdo.PersistenceManager pm = wc.getPm();
	RefObject_1_0 obj = wc.getObject();
	int tabIndex = 0;
	boolean mustReload = false;
%>
<!DOCTYPE HTML PUBLIC "-//W3C//DTD HTML 4.01 Transitional//EN" "http://www.w3.org/TR/html4/loose.dtd">
<html>
<head>
	<title><%= app.getApplicationName() %> - Connection URLs for Groupware</title>
	<meta http-equiv="Content-Type" content="text/html; charset=utf-8">
	<!-- Styles -->
	<link rel="stylesheet" href="../../js/bootstrap/css/bootstrap.min.css">	
	<link rel="stylesheet" href="../../_style/colors.css">
	<link rel="stylesheet" href="../../_style/n2default.css">
	<link rel="stylesheet" href="../../_style/ssf.css">

	<!-- Libraries -->
    <script src="../../js/prototype.js"></script>
    <script src="../../js/jquery/jquery.min.js"></script>
	<script>
	  $.noConflict();
	</script>
	<script src="../../js/bootstrap/js/bootstrap.min.js"></script>
	<script src="../../js/portal-all.js"></script>	
	<link rel='shortcut icon' href='../../images/favicon.ico' />

	<style type="text/css" media="all">
	    TABLE.fieldGroup TD {
	      vertical-align:middle;
	    }
	    .label {
	      width:190px;
	    }
  	</style>
</head>
   <body onload="initPage();">
   <div id="container">
     <div id="wrap">
       <div id="header" style="height:90px;">
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
       <div id="content">
         <form name="ConnectionHelper" accept-charset="UTF-8" method="POST" action="<%= "../.." + request.getServletPath() %>">
           <input type="hidden" name="<%= Action.PARAMETER_OBJECTXRI %>" value="<%= wc.getObjectIdentity().toXRI() %>" />
           <input type="hidden" name="<%= Action.PARAMETER_REQUEST_ID %>" value="<%= wc.getRequestId() %>" />
           <input type="checkbox" style="display:none;" id="isInitialized" name="isInitialized" checked />
           <div class="fieldGroupName">Resource</div>
			<div>
               <table class="fieldGroup">
                 <tr>
<%
                    {
%>
                        <td class="<%= CssClass.fieldLabel %>"><span class="nw">Selector type:</span></td>
                        <td>
                            <select class="valueL" id="selectorType" name="selectorType" class="valueL" tabindex="<%= tabIndex + 10 %>" onchange="javascript:$('Reload.button').click();">
                                <option <%= wc.getSelectorType() == SelectorType.CALENDARPROFILE ? "selected" : "" %> value="<%= SelectorType.CALENDARPROFILE %>"><%= app.getLabel(ConnectionHelperController.CALENDARPROFILE_CLASS) %></option>
                                <option <%= wc.getSelectorType() == SelectorType.CARDPROFILE ? "selected" : "" %> value="<%= SelectorType.CARDPROFILE %>"><%= app.getLabel(ConnectionHelperController.CARDPROFILE_CLASS) %></option>
                                <option <%= wc.getSelectorType() == SelectorType.DOCUMENTPROFILE ? "selected" : "" %> value="<%= SelectorType.DOCUMENTPROFILE %>"><%= app.getLabel(ConnectionHelperController.DOCUMENTPROFILE_CLASS) %></option>
                            </select>
                        </td>
                        <td class="addon"></td>
<%
                    } 
%>
                  </tr>
                  <tr>
                    <td class="<%= CssClass.fieldLabel %>"><span class="nw"><%= wc.getAnchorObjectLabel() %>:</span></td>
                    <td>
<%
                        if(wc.getAnchorObjects() == null || wc.getAnchorObjects().isEmpty()) {
%>
                            <select class="valueL" id="anchorObjectXri" name="anchorObjectXri" class="valueL" tabindex="<%= tabIndex + 10 %>" onchange="javascript:$('Reload.button').click();">
                                <option value="">--</option>
                            </select>
<%
                        } else {
%>
                            <select class="valueL" id="anchorObjectXri" name="anchorObjectXri" class="valueL" tabindex="<%= tabIndex + 10 %>" onchange="javascript:$('Reload.button').click();">
<%
                                boolean hasSelection = false;
                                for (Iterator<String> i = wc.getAnchorObjects().keySet().iterator(); i.hasNext();) {
                                    String key = i.next();
                                    String value = wc.getAnchorObjects().get(key);
                                    boolean selected = 
                                    	((wc.getAnchorObjectXri() != null) && (value != null) && (wc.getAnchorObjectXri().equals(value)));
                                    if (selected) {
                                        hasSelection = true;
                                    }
%>
	                                  <option <%= selected ? "selected" : "" %> value="<%= value != null ? value : "" %>"><%= key %></option>
<%
	                              }
%>
	                          </select>
<%
                            if ((wc.getAnchorObjectXri() != null) && !wc.getAnchorObjectXri().isEmpty() && (!hasSelection)) {
                                mustReload = true;
                            }
                        }
%>
                    </td>
                    <td class="addon"></td>
                  </tr>
                </table>
            </div>
<%
			RefObject_1_0 anchorObject = obj;
			if(wc.getAnchorObjectXri() != null && !wc.getAnchorObjectXri().isEmpty()) {
				anchorObject = (RefObject_1_0)pm.getObjectById(new Path(wc.getAnchorObjectXri()));
			}
			boolean showOptionIsDisabled = false;
			boolean showOptionMax = false;
			boolean showOptionUser = false;
			boolean showOptionSummaryPrefix = false;
			boolean showOptionCategories = false;
			boolean showOptionYear = false;
			boolean showOptionAlarm = false;
			boolean showOptionTimelineHeight = false;
			
			List<AdapterConnectionHelper.ConnectionURL> urls = org.opencrx.application.utils.AdapterConnectionHelper.getCalDavCollectionSetURLs(wc.getUrlBase(), anchorObject);
			if(!urls.isEmpty()) {
%>
				<br />
				<div class="fieldGroupName">CalDAV Calendar Home (use with CalDAV clients only)</div>
				<br />
<%				
	            for(AdapterConnectionHelper.ConnectionURL connectionURL: urls) {
%>
    	            <a href="<%= connectionURL.getUrl() %>" target="_blank" title="<%= app.getPortalExtension().getTitle(connectionURL.getObject(), (short)0, null, false, app) %>"><%= connectionURL.getUrl() %></a>
    	            <br />
<%
        	    }
			}
            urls = org.opencrx.application.utils.AdapterConnectionHelper.getCalDavEventCollectionURLs(wc.getUrlBase(), anchorObject);
            if(!urls.isEmpty()) {
%>
				<br />
				<div class="fieldGroupName">CalDAV Event Collections</div>
				<br />
<%				
				for(AdapterConnectionHelper.ConnectionURL connectionURL: urls) {
					String stringifiedURL = connectionURL.getUrl().toString();
					stringifiedURL = stringifiedURL.endsWith("/") ? stringifiedURL : stringifiedURL + "/";
%>
                	<a href="<%= stringifiedURL %>" target="_blank" title="<%= app.getPortalExtension().getTitle(connectionURL.getObject(), (short)0, null, false, app) %>"><%= stringifiedURL %></a>
                	<br />
<%
				}
            }
			urls = org.opencrx.application.utils.AdapterConnectionHelper.getCalDavTaskCollectionURLs(wc.getUrlBase(), anchorObject);
			if(!urls.isEmpty()) {
%>
				<br />
				<div class="fieldGroupName">CalDAV Task Collections</div>
				<br />
<%				
				for(AdapterConnectionHelper.ConnectionURL connectionURL: urls) {
					String stringifiedURL = connectionURL.getUrl().toString();
					stringifiedURL = stringifiedURL.endsWith("/") ? stringifiedURL : stringifiedURL + "/";
%>
                	<a href="<%= stringifiedURL %>" target="_blank" title="<%= app.getPortalExtension().getTitle(connectionURL.getObject(), (short)0, null, false, app) %>"><%= stringifiedURL %></a>
                	<br />
<%
				}
			}
			urls = org.opencrx.application.utils.AdapterConnectionHelper.getWebDavCollectionSetURLs(wc.getUrlBase(), anchorObject);
			if(!urls.isEmpty()) {
%>
				<br />
				<div class="fieldGroupName">WebDAV Collections Home (use with WebDAV clients only)</div>
				<br />
<%				
	            for(AdapterConnectionHelper.ConnectionURL connectionURL: urls) {
%>
    	            <a href="<%= connectionURL.getUrl() %>" target="_blank" title="<%= app.getPortalExtension().getTitle(connectionURL.getObject(), (short)0, null, false, app) %>"><%= connectionURL.getUrl() %></a>
    	            <br />
<%
        	    }
			}			
			urls = org.opencrx.application.utils.AdapterConnectionHelper.getWebDavCollectionURLs(wc.getUrlBase(), anchorObject);
			if(!urls.isEmpty()) {
%>
				<br />
				<div class="fieldGroupName">WebDAV Collections</div>
				<br />
<%				
				for(AdapterConnectionHelper.ConnectionURL connectionURL: urls) {
					String stringifiedURL = connectionURL.getUrl().toString();
					stringifiedURL = stringifiedURL.endsWith("/") ? stringifiedURL : stringifiedURL + "/";
%>
                	<a href="<%= stringifiedURL %>" target="_blank" title="<%= app.getPortalExtension().getTitle(connectionURL.getObject(), (short)0, null, false, app) %>"><%= stringifiedURL %></a>
                	<br />
<%
				}
			}
			urls = org.opencrx.application.utils.AdapterConnectionHelper.getCardDavCollectionSetURLs(wc.getUrlBase(), anchorObject);
			if(!urls.isEmpty()) {
%>
				<br />
				<div class="fieldGroupName">CardDAV Addressbook Home (use with CardDAV clients only)</div>
				<br />
<%				
				for(AdapterConnectionHelper.ConnectionURL connectionURL: urls) {
%>
                	<a href="<%= connectionURL.getUrl() %>" target="_blank" title="<%= app.getPortalExtension().getTitle(connectionURL.getObject(), (short)0, null, false, app) %>"><%= connectionURL.getUrl() %></a>
                	<br />
<%
				}
			}
			urls = org.opencrx.application.utils.AdapterConnectionHelper.getCardDavCollectionURLs(wc.getUrlBase(), anchorObject);
			if(!urls.isEmpty()) {
%>
				<br />
				<div class="fieldGroupName">CardDAV Collections</div>
				<br />
<%				
				for(AdapterConnectionHelper.ConnectionURL connectionURL: urls) {
					String stringifiedURL = connectionURL.getUrl().toString();
					stringifiedURL = stringifiedURL.endsWith("/") ? stringifiedURL : stringifiedURL + "/";
%>
                	<a href="<%= stringifiedURL %>" target="_blank" title="<%= app.getPortalExtension().getTitle(connectionURL.getObject(), (short)0, null, false, app) %>"><%= stringifiedURL %></a>
                	<br />
<%
				}
			}
			urls = org.opencrx.application.utils.AdapterConnectionHelper.getICalURLs(
				wc.getUrlBase(), 
				anchorObject,
				Integer.toString(wc.getOptionMax()),
				Boolean.toString(wc.getOptionIsDisabled())
			);
			if(!urls.isEmpty()) {
%>
				<br />
				<div class="fieldGroupName">ICAL Calendars</div>
				<br />
<%				
				for(AdapterConnectionHelper.ConnectionURL connectionURL: urls) {
%>
                	<a href="<%= connectionURL.getUrl() %>" target="_blank" title="<%= app.getPortalExtension().getTitle(connectionURL.getObject(), (short)0, null, false, app) %>"><%= connectionURL.getUrl() %></a>
                	<br />
<%
				}
				showOptionIsDisabled = true;
				showOptionMax = true;
			}
			urls = org.opencrx.application.utils.AdapterConnectionHelper.getVCardURLs(wc.getUrlBase(), anchorObject);
			if(!urls.isEmpty()) {
%>
				<br />
				<div class="fieldGroupName">VCARD Collections</div>
				<br />
<%				
				for(AdapterConnectionHelper.ConnectionURL connectionURL: urls) {
%>
                	<a href="<%= connectionURL.getUrl() %>" target="_blank" title="<%= app.getPortalExtension().getTitle(connectionURL.getObject(), (short)0, null, false, app) %>"><%= connectionURL.getUrl() %></a>
                	<br />
<%
				}
			}
			urls = org.opencrx.application.utils.AdapterConnectionHelper.getOtherCalendarURLs(
				wc.getUrlBase(), 
				anchorObject,
				Integer.toString(wc.getOptionMax()),
				wc.getOptionSummaryPrefix(),
				wc.getOptionCategories(),
				Integer.toString(wc.getOptionYear()),
				Boolean.toString(wc.getOptionAlarm())
			);
			if(!urls.isEmpty()) {
%>
				<br />
				<div class="fieldGroupName">Other Calendars (Birthdays, Anniversaries, Dates of Death, ...)</div>
				<br />
<%				
				for(AdapterConnectionHelper.ConnectionURL connectionURL: urls) {
%>
                	<a href="<%= connectionURL.getUrl() %>" target="_blank" title="<%= app.getPortalExtension().getTitle(connectionURL.getObject(), (short)0, null, false, app) %>"><%= connectionURL.getUrl() %></a>
                	<br />
<%
				}
				showOptionMax = true;
				showOptionSummaryPrefix = true;
				showOptionCategories = true;
				showOptionYear = true;
				showOptionAlarm = true;
			}
			urls = org.opencrx.application.utils.AdapterConnectionHelper.getFreeBusyURLs(
				wc.getUrlBase(), 
				anchorObject,
				wc.getOptionUser(),
				Integer.toString(wc.getOptionMax()),
				Boolean.toString(wc.getOptionIsDisabled())
			);
			if(!urls.isEmpty()) {
%>
				<br />
				<div class="fieldGroupName">FreeBusy Calendars</div>
				<br />
<%				
				for(AdapterConnectionHelper.ConnectionURL connectionURL: urls) {
%>
                	<a href="<%= connectionURL.getUrl() %>" target="_blank" title="<%= app.getPortalExtension().getTitle(connectionURL.getObject(), (short)0, null, false, app) %>"><%= connectionURL.getUrl() %></a>
                	<br />
<%
				}
				showOptionIsDisabled = true;
				showOptionMax = true;
				showOptionUser = true;
			}
%>
			<br />
			<div class="fieldGroupName">Options</div>
			<br />
			<table class="fieldGroup">
<%
				if(showOptionUser) {
%>				
					<tr>
					    <td class="<%= CssClass.fieldLabel %>"><span class="nw">User:</span></td>
					    <td><input type="text" class="valueL" name="optionUser" value="<%= wc.getOptionUser() %>" onchange="javascript:$('Reload.button').click();"></input></td>
					    <td class="addon"></td>
					</tr>
<%
				}
				if(showOptionMax) {
%>					  
					<tr title="maximum number of accounts - default is '500'">
					    <td class="<%= CssClass.fieldLabel %>"><span class="nw">Max:</span></td>
					    <td><input type="text" class="valueL" name="optionMax" value="<%= Integer.toString(wc.getOptionMax()) %>" onchange="javascript:$('Reload.button').click();"></input></td>
					    <td class="addon"></td>
					</tr>
<%
				}
				if(showOptionIsDisabled) {
%>					  
					<tr title="activate filter 'disabled' to process disabled activities only">
						<td class="<%= CssClass.fieldLabel %>"><span class="nw">Disabled:</span></td>
					    <td>
							<select class="valueL" name="optionIsDisabled" onchange="javascript:$('Reload.button').click();">
								<option <%= Boolean.TRUE.equals(wc.getOptionIsDisabled()) ? "selected" : "" %> value="true">true</option>						
								<option <%= !Boolean.TRUE.equals(wc.getOptionIsDisabled()) ? "selected" : "" %> value="false">false</option>						
							</select>						
					    </td>
					    <td class="addon"></td>
					</tr>
<%
				}
				if(showOptionSummaryPrefix) {
%>					  
					<tr title="Summary prefix - default is ''">
					    <td class="<%= CssClass.fieldLabel %>"><span class="nw">Summary prefix:</span></td>
					    <td><input type="text" class="valueL" name="optionSummaryPrefix" value="<%= wc.getOptionSummaryPrefix() %>" onchange="javascript:$('Reload.button').click();"></input></td>
					    <td class="addon"></td>
					</tr>
<%
				}
				if(showOptionCategories) {
%>					  
					<tr>
					    <td class="<%= CssClass.fieldLabel %>"><span class="nw">Categories:</span></td>
					    <td><input type="text" class="valueL" name="optionCategories" value="<%= wc.getOptionCategories() %>" onchange="javascript:$('Reload.button').click();"></input></td>
					    <td class="addon"></td>
					</tr>
<%
				}
				if(showOptionYear) {
%>					  
					<tr title="generate data for year-1, year, year+1 - default is current year">
					    <td class="<%= CssClass.fieldLabel %>"><span class="nw">Year:</span></td>
					    <td><input type="text" class="valueL" name="optionYear" value="<%= Integer.toString(wc.getOptionYear()) %>" onchange="javascript:$('Reload.button').click();"></input></td>
					    <td class="addon"></td>
					</tr>
<%
				}
				if(showOptionTimelineHeight) {
%>					  
					<tr>
					    <td class="<%= CssClass.fieldLabel %>"><span class="nw">Timeline height (in pixels):</span></td>
					    <td><input type="text" class="valueL" name="optionTimelineHeight" value="<%= Integer.toString(wc.getOptionTimelineHeight()) %>" onchange="javascript:$('Reload.button').click();"></input></td>
					    <td class="addon"></td>
					</tr>
<%
				}
				if(showOptionAlarm) {
%>					  
					<tr>
						<td class="<%= CssClass.fieldLabel %>"><span class="nw">Alarm:</span></td>
					    <td>
							<select class="valueL" name="optionAlarm" onchange="javascript:$('Reload.button').click();">
								<option <%= Boolean.TRUE.equals(wc.getOptionAlarm()) ? "selected" : "" %> value="true">true</option>						
								<option <%= !Boolean.TRUE.equals(wc.getOptionAlarm()) ? "selected" : "" %> value="false">false</option>						
							</select>						
					    </td>
					    <td class="addon"></td>
					</tr>
<%
				}
%>					  
			</table>
			<br />
			<div class="fieldGroupName">Hints</div>
			<div>
				<p>
				See the <a href="http://www.opencrx.org/documents.htm" target="_blank"><strong>openCRX Admin Guide</strong></a> for 
				the component configuration of the CalDAV, CardDAV, ICAL and VCARD adapters.
				<p>
				The properties <strong>maxActivities</strong> and <strong>maxAccounts</strong> allow
				to configure the maximum number of items returned by the adapters (default is 500).
			</div>
			<br />
			<div class="fieldGroupName">&nbsp;</div>
            <input type="submit" id="Reload.button" name="Reload" class="<%= CssClass.btn.toString() %> <%= CssClass.btn_light.toString() %>" tabindex="<%= tabIndex++ %>" value="<%= app.getTexts().getReloadText() %>" />
            <input type="submit" id="Cancel" name="Cancel" class="<%= CssClass.btn.toString() %> <%= CssClass.btn_light.toString() %>" tabindex="30" value="<%= app.getTexts().getCancelTitle() %>"  onClick="javascript:window.close();" />
            <br />
         </form>
       </div> <!-- content -->
     </div> <!-- content-wrap -->
   </div> <!-- wrap -->
  </div> <!-- container -->
  <script language="javascript" type="text/javascript">
	function initPage() {
<%
		if (mustReload) {
%>
			$('Reload.button').click();
<%
		}
%>
	}
  </script>
</body>
</html>
<t:wizardClose controller="<%= wc %>" />
    