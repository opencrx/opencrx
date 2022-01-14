<%@  page contentType="text/html;charset=UTF-8" language="java" pageEncoding="UTF-8" %><%
/*
 * ====================================================================
 * Project:     openmdx, http://www.openmdx.org/
 * Description: seek relationships between accounts
 * Owner:       the original authors.
 * ====================================================================
 *
 * This software is published under the BSD license
 * as listed below.
 * * Redistribution and use in source and binary forms, with or without
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
java.util.SortedSet,
java.util.TreeSet,
java.util.List,
java.util.ArrayList,
java.util.Collections,
java.util.*,
java.io.*,
java.sql.*,
java.text.*,
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
org.openmdx.base.naming.*
" %><%
	request.setCharacterEncoding("UTF-8");
	ApplicationContext app = (ApplicationContext)session.getValue(WebKeys.APPLICATION_KEY);
	ViewsCache viewsCache = (ViewsCache)session.getValue(WebKeys.VIEW_CACHE_KEY_SHOW);
	String requestId =  request.getParameter(Action.PARAMETER_REQUEST_ID);
	String requestIdParam = Action.PARAMETER_REQUEST_ID + "=" + requestId;
	String objectXri = request.getParameter(Action.PARAMETER_OBJECTXRI);
	String xriParam = Action.PARAMETER_OBJECTXRI + "=" + objectXri;
	if (objectXri == null || app == null || viewsCache.getView(requestId) == null) {
		response.sendRedirect(
		   request.getContextPath() + "/" + WebKeys.SERVLET_NAME
		);
		return;
	}
	javax.jdo.PersistenceManager pm = app.getNewPmData();
	Texts_1_0 texts = app.getTexts();
	Codes codes = app.getCodes();

  Map<Short,String> allQualityCodes = codes.getLongTextByCode("memberQuality", app.getCurrentLocaleAsIndex(), true);
  Map<Short,String> qualityCodes = codes.getLongTextByCode("memberQuality", app.getCurrentLocaleAsIndex(), false);

	final String ACCOUNT_MEMBERSHIP_CLASS = "org:opencrx:kernel:account1:AccountMembership";
	final String ACCOUNT_CLASS = "org:opencrx:kernel:account1:Account";
	final String CONTACT_CLASS = "org:opencrx:kernel:account1:Contact";
	final String GROUP_CLASS = "org:opencrx:kernel:account1:Group";

	final String formName   = "AccountRelationships";
	final String wizardName = formName + ".jsp";
	final String wizardRelationshipGraph = formName + "Graph.jsp";
	NumberFormat formatter = new DecimalFormat("0");
	NumberFormat formatter5 = new DecimalFormat("00000");

  // References
  final String accountTargetFinder = "org:opencrx:kernel:account1:Segment:account";

  // get Parameters
  String accountTargetXriTitle = (request.getParameter("accountTargetXri.Title") == null ? "" : request.getParameter("accountTargetXri.Title"));
  String accountTargetXri = (request.getParameter("accountTargetXri") == null ? "" : request.getParameter("accountTargetXri"));
  org.opencrx.kernel.account1.jmi1.Account accountTarget = null;
  if (accountTargetXri.length() > 0) {
    try {
      accountTarget = (org.opencrx.kernel.account1.jmi1.Account)pm.getObjectById(new Path(accountTargetXri));
    } catch (Exception e) {
      accountTargetXri = "";
    }
  }
	final int maxHopsSupported = 6;
	String maxHopsPara = request.getParameter("maxHopsPara") != null ? request.getParameter("maxHopsPara") : "2"; // default is 2 hops
  int maxHops = 0; try {maxHops = Integer.parseInt(maxHopsPara);} catch (Exception e) {}
	String minQualityPara   = request.getParameter("minQualityPara") != null ? request.getParameter("minQualityPara") : "9";
  int minQuality = 0; try {minQuality = Integer.parseInt(minQualityPara);} catch (Exception e) {}
	boolean actionOk        = request.getParameter("OK.Button") != null;
	boolean actionCancel    = request.getParameter("Cancel.Button") != null;

  // Get data package. This is the JMI root package to handle
  // openCRX object requests
  RefObject_1_0 obj = (RefObject_1_0)pm.getObjectById(new Path(objectXri));

  org.opencrx.kernel.account1.jmi1.Account accountSource = null;
  if (obj instanceof org.opencrx.kernel.account1.jmi1.Account) {
    accountSource = (org.opencrx.kernel.account1.jmi1.Account)obj;
  }

	if(actionCancel) {
		Action nextAction = new ObjectReference(obj, app).getSelectObjectAction();
		response.sendRedirect(
			request.getContextPath() + "/" + nextAction.getEncodedHRef()
  	);
  	return;
	}
	UserDefinedView userView = new UserDefinedView(
		obj,
		app,
		viewsCache.getView(requestId)
	);

%>
<!DOCTYPE HTML PUBLIC "-//W3C//DTD HTML 4.01 Transitional//EN" "http://www.w3.org/TR/html4/loose.dtd">
<html dir="<%= texts.getDir() %>">
<head>
  <title>Relationships</title>
  <meta http-equiv="Content-Type" content="text/html; charset=utf-8">
  <link href="../../_style/n2default.css" rel="stylesheet" type="text/css">
  <link href="../../_style/ssf.css" rel="stylesheet" type="text/css">
  <link href="../../_style/colors.css" rel="stylesheet" type="text/css">
  <!--[if lt IE 7]><script type="text/javascript" src="../../js/iehover-fix.js"></script><![endif]-->
  <script language="javascript" type="text/javascript" src="../../js/portal-all.js"></script>

  <script language="javascript" type="text/javascript">
    var OF = null;
    try {
      OF = self.opener.OF;
    }
    catch(e) {
      OF = null;
    }
    if(!OF) {
      OF = new ObjectFinder();
    }
  </script>
  <link rel="shortcut icon" href="../../images/favicon.ico" />

  <STYLE type="text/css">
    INPUT.inputError {
      background-color: #CCFFA4;
    }
 </STYLE>
</head>

<body>
<div id="container">
	<div id="wrap">
		<div id="eheader" style="height:90px;">
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
<%
	try {
    Path objectPath = new Path(objectXri);
    String providerName = objectPath.get(2);
    String segmentName = objectPath.get(4);
%>

<form name="<%= formName %>" accept-charset="UTF-8" method="POST" action="<%= "../.." + request.getServletPath() %>">
  <input type="hidden" name="<%= Action.PARAMETER_OBJECTXRI %>" value="<%= objectXri %>" />
  <input type="hidden" name="<%= Action.PARAMETER_REQUEST_ID %>" value="<%= requestId %>" />
  <table cellspacing="8" class="tableLayout">
    <tr>
      <td class="cellObject">
        <noscript>
          <div class="panelJSWarning" style="display: block;">
            <a href="../../helpJsCookie.html" target="_blank"><img class="popUpButton" src="../../images/help.gif" width="16" height="16" border="0" onclick="javascript:void(window.open('helpJsCookie.html', 'Help', 'fullscreen=no,toolbar=no,status=no,menubar=no,scrollbars=yes,resizable=yes,directories=no,location=no,width=400'));" alt="" /></a> <%= texts.getPageRequiresScriptText() %>
          </div>
        </noscript>
        <div id="etitle" style="height:20px;">
           Relationships
        </div>

        <fieldset>
  	      <table class="fieldGroup">
  	        <tr>
            	<td class="<%= CssClass.fieldLabel %>"><span class="nw">&nbsp;</span></td>
  	          <td></td>
  	          <td class="addon"></td>
            	<td class="<%= CssClass.fieldLabel %>"><span class="nw">&nbsp;</span></td>
  	          <td></td>
  	          <td class="addon"></td>
  	        </tr>

  	        <tr>
            	<td class="<%= CssClass.fieldLabel %>"><span class="nw"><%= app.getLabel(ACCOUNT_CLASS) %> #1:</span></td>
  	          <td colspan=4><b><%= accountSource == null ? "???" : (new ObjectReference(accountSource, app)).getTitle() %></b></td>
  	          <td class="addon"></td>
  	        </tr>

   	        <tr>
            	<td class="<%= CssClass.fieldLabel %>"><span class="nw"><%= app.getLabel(ACCOUNT_CLASS) %> #2: <font color="red">*</font></span></td>
<%
              String lookupId = org.opencrx.kernel.backend.Contracts.getInstance().getUidAsString();
              Action findAccountTargetObjectAction = Action.getFindObjectAction(accountTargetFinder, lookupId);
              String accountName = app.getLabel(ACCOUNT_CLASS);
%>
  	          <td colspan=4>
                <div class="autocompleterMenu">
                  <ul id="<%=CssClass.ssf_nav %>" class="<%=CssClass.ssf_nav %>" onmouseover="sfinit(this);" >
                    <li><a href="#"><img border="0" alt="" src="../../images/autocomplete_select.png" /></a>
                      <ul onclick="this.style.left='-999em';" onmouseout="this.style.left='';">
                        <li class="selected"><a href="#" onclick="javascript:navSelect(this);ac_addObject0.url= './'+getEncodedHRef(['../../ObjectInspectorServlet', 'event', '40', 'parameter', 'xri*(xri:@openmdx:org.opencrx.kernel.account1/provider/<%= providerName %>/segment/<%= segmentName %>)*referenceName*(account)*filterByType*(org:opencrx:kernel:account1:Account)*filterByFeature*(fullName)*filterOperator*(IS_LIKE)*orderByFeature*(fullName)*position*(0)*size*(20)']);return false;"><span>&nbsp;&nbsp;&nbsp;</span><%= accountName %> / <%= userView.getFieldLabel(ACCOUNT_CLASS, "fullName", app.getCurrentLocaleAsIndex()) %></a></li>
                        <li><a href="#" onclick="javascript:navSelect(this);ac_addObject0.url= './'+getEncodedHRef(['../../ObjectInspectorServlet', 'event', '40', 'parameter', 'xri*(xri:@openmdx:org.opencrx.kernel.account1/provider/<%= providerName %>/segment/<%= segmentName %>)*referenceName*(account)*filterByType*(org:opencrx:kernel:account1:Account)*filterByFeature*(description)*filterOperator*(IS_LIKE)*orderByFeature*(description)*position*(0)*size*(20)']);return false;"><span>&nbsp;&nbsp;&nbsp;</span><%= accountName %> / <%= userView.getFieldLabel(ACCOUNT_CLASS, "description", app.getCurrentLocaleAsIndex()) %></a></li>
                        <li><a href="#" onclick="javascript:navSelect(this);ac_addObject0.url= './'+getEncodedHRef(['../../ObjectInspectorServlet', 'event', '40', 'parameter', 'xri*(xri:@openmdx:org.opencrx.kernel.account1/provider/<%= providerName %>/segment/<%= segmentName %>)*referenceName*(account)*filterByType*(org:opencrx:kernel:account1:Account)*filterByFeature*(aliasName)*filterOperator*(IS_LIKE)*orderByFeature*(aliasName)*position*(0)*size*(20)']);return false;"><span>&nbsp;&nbsp;&nbsp;</span><%= accountName %> / <%= userView.getFieldLabel(ACCOUNT_CLASS, "aliasName", app.getCurrentLocaleAsIndex()) %></a></li>
                        <li><a href="#" onclick="javascript:navSelect(this);ac_addObject0.url= './'+getEncodedHRef(['../../ObjectInspectorServlet', 'event', '40', 'parameter', 'xri*(xri:@openmdx:org.opencrx.kernel.account1/provider/<%= providerName %>/segment/<%= segmentName %>)*referenceName*(account)*filterByType*(org:opencrx:kernel:account1:Account)*filterByFeature*(firstName)*filterOperator*(IS_LIKE)*orderByFeature*(firstName)*position*(0)*size*(20)']);return false;"><span>&nbsp;&nbsp;&nbsp;</span><%= accountName %> / <%= userView.getFieldLabel(CONTACT_CLASS, "firstName", app.getCurrentLocaleAsIndex()) %></a></li>
                        <li><a href="#" onclick="javascript:navSelect(this);ac_addObject0.url= './'+getEncodedHRef(['../../ObjectInspectorServlet', 'event', '40', 'parameter', 'xri*(xri:@openmdx:org.opencrx.kernel.account1/provider/<%= providerName %>/segment/<%= segmentName %>)*referenceName*(account)*filterByType*(org:opencrx:kernel:account1:Account)*filterByFeature*(middleName)*filterOperator*(IS_LIKE)*orderByFeature*(middleName)*position*(0)*size*(20)']);return false;"><span>&nbsp;&nbsp;&nbsp;</span><%= accountName %> / <%= userView.getFieldLabel(CONTACT_CLASS, "middleName", app.getCurrentLocaleAsIndex()) %></a></li>
                        <li><a href="#" onclick="javascript:navSelect(this);ac_addObject0.url= './'+getEncodedHRef(['../../ObjectInspectorServlet', 'event', '40', 'parameter', 'xri*(xri:@openmdx:org.opencrx.kernel.account1/provider/<%= providerName %>/segment/<%= segmentName %>)*referenceName*(account)*filterByType*(org:opencrx:kernel:account1:Account)*filterByFeature*(lastName)*filterOperator*(IS_LIKE)*orderByFeature*(lastName)*position*(0)*size*(20)']);return false;"><span>&nbsp;&nbsp;&nbsp;</span><%= accountName %> / <%= userView.getFieldLabel(CONTACT_CLASS, "lastName", app.getCurrentLocaleAsIndex()) %></a></li>
                        <li><a href="#" onclick="javascript:navSelect(this);ac_addObject0.url= './'+getEncodedHRef(['../../ObjectInspectorServlet', 'event', '40', 'parameter', 'xri*(xri:@openmdx:org.opencrx.kernel.account1/provider/<%= providerName %>/segment/<%= segmentName %>)*referenceName*(account)*filterByType*(org:opencrx:kernel:account1:Account)*filterByFeature*(name)*filterOperator*(IS_LIKE)*orderByFeature*(name)*position*(0)*size*(20)']);return false;"><span>&nbsp;&nbsp;&nbsp;</span><%= accountName %> / <%= userView.getFieldLabel(GROUP_CLASS, "name", app.getCurrentLocaleAsIndex()) %></a></li>
                        <li><a href="#" onclick="javascript:navSelect(this);ac_addObject0.url= './'+getEncodedHRef(['../../ObjectInspectorServlet', 'event', '40', 'parameter', 'xri*(xri:@openmdx:org.opencrx.kernel.account1/provider/<%= providerName %>/segment/<%= segmentName %>)*referenceName*(account)*filterByType*(org:opencrx:kernel:account1:Account)*filterByFeature*(nickName)*filterOperator*(IS_LIKE)*orderByFeature*(nickName)*position*(0)*size*(20)']);return false;"><span>&nbsp;&nbsp;&nbsp;</span><%= accountName %> / <%= userView.getFieldLabel(CONTACT_CLASS, "nickName", app.getCurrentLocaleAsIndex()) %></a></li>
                      </ul>
                    </li>
                  </ul>
                </div>
                <div class="autocompleterInput"><input type="text" class="valueL valueAC <%= actionOk && (accountTarget == null) ? "inputError" : "" %>" id="accountTargetXri.Title" name="accountTargetXri.Title" tabindex="100" value="<%= accountTargetXriTitle != null ? accountTargetXriTitle : "" %>" /></div>
                <input type="hidden" class="valueLLocked" id="accountTargetXri" name="accountTargetXri" readonly value="<%= accountTargetXri != null ? accountTargetXri : "" %>" />
                <div class="autocomplete" id="accountTarget.Update" style="display:none;z-index:500;"></div>
                <script type="text/javascript" language="javascript" charset="utf-8">
                  ac_addObject0 = new Ajax.Autocompleter(
                    'accountTargetXri.Title',
                    'accountTarget.Update',
                    '../../ObjectInspectorServlet?event=40&parameter=xri*%28xri%3A%40openmdx%3Aorg.opencrx.kernel.account1%2Fprovider%2F<%= providerName %>%2Fsegment%2F<%= segmentName %>%29*referenceName*%28account%29*filterByType*%28org%3Aopencrx%3Akernel%3Aaccount1%3AAccount%29*filterByFeature*%28fullName%29*filterOperator*%28IS_LIKE%29*orderByFeature*%28fullName%29*position*%280%29*size*%2820%29',
                    {
                      paramName: 'filtervalues',
                      minChars: 0,
                      afterUpdateElement: updateXriField
                    }
                  );
                </script>
  	          </td>
  	          <td class="addon">
                <img class="popUpButton" border="0" align="bottom" alt="Click to open ObjectFinder" src="../../images/lookup.gif" onclick="OF.findObject('../../<%= findAccountTargetObjectAction.getEncodedHRef() %>', $('accountTargetXri.Title'), $('accountTargetXri'), '<%= lookupId %>');" />
  	          </td>
            </tr>

  	        <tr>
            	<td class="<%= CssClass.fieldLabel %>"><span class="nw">Max. <%= userView.getFieldLabel(ACCOUNT_MEMBERSHIP_CLASS, "distance", app.getCurrentLocaleAsIndex()) %></span></td>
  	          <td>
                <select class="valueL" name="maxHopsPara" tabindex="110">
<%
                  for(int i=2; i<=maxHopsSupported; i+=2) {
%>
                    <option <%= maxHops == i ? "selected" : "" %> value="<%= i %>"><%= i %>
<%
                  }
%>
                </select>
  	          </td>
  	          <td class="addon"></td>

            	<td class="<%= CssClass.fieldLabel %>"><span class="nw">Min. <%= userView.getFieldLabel(ACCOUNT_MEMBERSHIP_CLASS, "quality", app.getCurrentLocaleAsIndex()) %></span></td>
  	          <td>
                <select class="valueL" name="minQualityPara" tabindex="120">
<%
                  if (qualityCodes == null) {
%>
                    <option value="0">N/A
<%
                  }
                  else {
                    for(Iterator options = qualityCodes.entrySet().iterator(); options.hasNext(); ) {
                      Map.Entry option = (Map.Entry)options.next();
                      Short value = new Short(((Number)option.getKey()).shortValue());
                      String selectedModifier = minQuality == value.intValue() ? "selected" : "";
%>
                      <option <%= selectedModifier %> value="<%= value %>"><%= allQualityCodes == null ? value : allQualityCodes.get(value) %>
<%
                    }
                  }
%>
                </select>
  	          </td>
  	          <td class="addon"></td>
  	        </tr>
          </table>
        </fieldset>
    	</td>
    </tr>
  </table>
  <table class="fieldGroup">
    <tr id="submitButtons" style="font-weight:bold;">
      <td>
        <input type="Submit" name="OK.Button" id="OK.Button" class="<%= CssClass.btn.toString() %> <%= CssClass.btn_light.toString() %>" tabindex="9000" value="<%= app.getTexts().getOkTitle() %>" onmouseup="javascript:$('waitMsg').style.display='block';$('submitButtons').style.visibility='hidden';" />
        <input type="Submit" name="Cancel.Button" class="<%= CssClass.btn.toString() %> <%= CssClass.btn_light.toString() %>" tabindex="9010" value="<%= app.getTexts().getCancelTitle() %>" onmouseup="javascript:$('waitMsg').style.display='block';$('submitButtons').style.visibility='hidden';" />
      </td>
    </tr>
    <tr id="waitMsg" style="display:none;">
      <td>
        <div style="padding-left:5px; padding-bottom: 3px;">
          <img src="../../images/wait.gif" alt="" />
        </div>
      </td>
    </tr>
  </table>


<%
	if (actionOk && (accountSource != null) && (accountTarget != null)) {
    // idea: get all memberships

    // initialize account sets
    Set accountsReachedFromSource = new HashSet();
    Set accountsReachedFromTarget = new HashSet();
    accountsReachedFromSource.add(new String(((new Path(accountSource.refMofId())).getLastSegment()).toString()));
    accountsReachedFromTarget.add(new String(((new Path(accountTarget.refMofId())).getLastSegment()).toString()));
    int previousSourceSetSize = 0;
    int previousTargetSetSize = 0;

    // calculate intersection
    Set intersection = new HashSet(accountsReachedFromSource);
    intersection.retainAll(accountsReachedFromTarget);

    int currentDistance = 0;
    boolean expandSourceSet = false; // source and target sets are expanded in an alternating fashion

    //System.out.println("------starting search....");
    while (
      intersection.isEmpty() &&
      (currentDistance < maxHops) &&
      (
        (previousSourceSetSize < accountsReachedFromSource.size()) || // new accounts reachable from source found
        (previousTargetSetSize < accountsReachedFromTarget.size())    // new accounts reachable from target found
      )
    ) {
      //System.out.println("------NEW LOOP-----  accountsReachedFromSource=" + formatter.format(accountsReachedFromSource.size()) + " / accountsReachedFromTarget=" + formatter.format(accountsReachedFromTarget.size()));

      Set currentAccountSet = null;;
      expandSourceSet = !expandSourceSet;
      if (expandSourceSet) {
        if (previousSourceSetSize < accountsReachedFromSource.size()) {
          // source set was expanded --> do another hop
          currentAccountSet = new HashSet(accountsReachedFromSource);
          previousSourceSetSize = accountsReachedFromSource.size();
        }
      } else {
        if (previousTargetSetSize < accountsReachedFromTarget.size()) {
          // source set was expanded --> do another hop
          currentAccountSet = new HashSet(accountsReachedFromTarget);
          previousTargetSetSize = accountsReachedFromTarget.size();
        }
      }

      if (currentAccountSet != null) {
        currentDistance += 1;

    		for(Iterator i = currentAccountSet.iterator(); i.hasNext(); ) {
    		  String accountQualifier = (String)i.next();
    		  org.opencrx.kernel.account1.jmi1.Account currentAccount =
            (org.opencrx.kernel.account1.jmi1.Account)pm.getObjectById(
              new Path("xri:@openmdx:org.opencrx.kernel.account1/provider/" + providerName + "/segment/" + segmentName + "/account/" + accountQualifier)
             );

			// get inbound memberships (i.e. distance == -1)
			org.opencrx.kernel.account1.cci2.AccountMembershipQuery membershipQuery = (org.opencrx.kernel.account1.cci2.AccountMembershipQuery)pm.newQuery(org.opencrx.kernel.account1.jmi1.AccountMembership.class);
			membershipQuery.forAllDisabled().isFalse();
			membershipQuery.distance().equalTo(new Short((short)-1));
			membershipQuery.quality().lessThanOrEqualTo(new Short((short)minQuality)); // will fail if quality IS NULL
			org.openmdx.base.rest.cci.QueryExtensionRecord queryFilterDM1 = org.openmdx.base.persistence.cci.PersistenceHelper.newQueryExtension(membershipQuery);
    			// HINT_DBOBJECT allows to qualify the DbObject to use.
    			// For distance 1 memberships use ACCTMEMBERSHIP1 instead of ACCTMEMBERSHIP
			queryFilterDM1.setClause(
	            "(" + org.openmdx.base.dataprovider.layer.persistence.jdbc.spi.Database_1_Attributes.HINT_DBOBJECT + "1 */ (1=1) ) and " +
	            "( " +
	            "v.member IN ( " +
	            "  select distinct(member) from oocke1_tobj_acctmembership1 m, oocke1_account a " +
	            "  where " +
	            "   ((m.disabled is null) or (m.disabled = '0')) and " +
	            "   ((m.account_to   = a.object_id) and ((a.disabled is null) or (a.disabled = '0'))) " +
	            "  ) " +
	            ") "
			);
      		Collection memberships = currentAccount.getAccountMembership(membershipQuery);
      		for(Iterator m = memberships.iterator(); m.hasNext(); ) {
      			org.opencrx.kernel.account1.jmi1.AccountMembership membership = (org.opencrx.kernel.account1.jmi1.AccountMembership)m.next();
      			int distance = membership.getDistance();
            int quality = 5;
            try {quality = membership.getQuality();} catch (Exception e) {}
      			if (
      			  (quality <= minQuality) &&
      			  (membership.getAccountFromId() != null)
      			) {
      			  // add this account to the appropriate account set
              String acctQualifier = membership.getAccountFromId().substring(membership.getAccountFromId().lastIndexOf("/") + 1);
              if (expandSourceSet) {
        			  //System.out.println("[dist=" + formatter5.format(currentDistance) + "] adding inbound to source set: " + ((new ObjectReference(membership.getAccountFrom(), app)).getTitle()));
                //accountsReachedFromSource.add(membership.getAccountFromId());
                //accountsReachedFromSource.add(new String(((new Path(membership.getAccountFrom().refMofId())).getLastSegment()).toString()));
                accountsReachedFromSource.add(acctQualifier);
              } else {
        			  //System.out.println("[dist=" + formatter5.format(currentDistance) + "] adding inbound to target set: " + ((new ObjectReference(membership.getAccountFrom(), app)).getTitle()));
                //accountsReachedFromTarget.add(new String(((new Path(membership.getAccountFrom().refMofId())).getLastSegment()).toString()));
                accountsReachedFromTarget.add(acctQualifier);
              }
            }
          }

			// get outbound memberships (i.e. distance == 1)
			membershipQuery = (org.opencrx.kernel.account1.cci2.AccountMembershipQuery)pm.newQuery(org.opencrx.kernel.account1.jmi1.AccountMembership.class);
			membershipQuery.forAllDisabled().isFalse();
      		membershipQuery.distance().equalTo(new Short((short)1));
      		membershipQuery.quality().lessThanOrEqualTo(new Short((short)minQuality)); // will fail if quality IS NULL
      		memberships = currentAccount.getAccountMembership(membershipQuery);
			// HINT_DBOBJECT allows to qualify the DbObject to use.
			// For distance 1 memberships use ACCTMEMBERSHIP1 instead of ACCTMEMBERSHIP
			org.openmdx.base.rest.cci.QueryExtensionRecord queryFilterD1 = org.openmdx.base.persistence.cci.PersistenceHelper.newQueryExtension(membershipQuery);
			queryFilterD1.setClause(
					org.openmdx.base.dataprovider.layer.persistence.jdbc.spi.Database_1_Attributes.HINT_DBOBJECT + "1 */ (1=1)"
			);
      		for(Iterator m = memberships.iterator(); m.hasNext(); ) {
      			org.opencrx.kernel.account1.jmi1.AccountMembership membership = (org.opencrx.kernel.account1.jmi1.AccountMembership)m.next();
      			int distance = membership.getDistance();
            int quality = 5;
            try {quality = membership.getQuality();} catch (Exception e) {}
      			if (
      			  (quality <= minQuality) &&
      			  (membership.getAccountToId() != null)
      			) {
              String acctQualifier = membership.getAccountToId().substring(membership.getAccountToId().lastIndexOf("/") + 1);
      			  // add this account to the appropriate account set
              if (expandSourceSet) {
        			  //System.out.println("[dist=" + formatter5.format(currentDistance) + "] adding outbound to source set: " + ((new ObjectReference(membership.getAccountTo(), app)).getTitle()));
                //accountsReachedFromSource.add(new String(((new Path(membership.getAccountTo().refMofId())).getLastSegment()).toString()));
                accountsReachedFromSource.add(acctQualifier);
              } else {
        			  //System.out.println("[dist=" + formatter5.format(currentDistance) + "] adding outbound to target set: " + ((new ObjectReference(membership.getAccountTo(), app)).getTitle()));
                //accountsReachedFromTarget.add(new String(((new Path(membership.getAccountTo().refMofId())).getLastSegment()).toString()));
                accountsReachedFromTarget.add(acctQualifier);
              }
            }
          }

          // calculate new intersection
          intersection = new HashSet(accountsReachedFromSource);
          intersection.retainAll(accountsReachedFromTarget);
        } /* for loop over each account in set */
      }
    } /* while loop */
%>
    <br />
    <br />
    <div style="padding:5px;border:1px solid black;">
<%
      if (intersection.isEmpty()) {
%>
        no relationship found
<%
      } else {
%>
        relationship<%= intersection.size() > 1 ? "s" : "" %> with <%= formatter.format(currentDistance) %> hops through:<br>
        <ul>
<%
    		for(Iterator i = intersection.iterator(); i.hasNext(); ) {
    		  String accountQualifier = (String)i.next();
    		  org.opencrx.kernel.account1.jmi1.Account account =
            (org.opencrx.kernel.account1.jmi1.Account)pm.getObjectById(
              new Path("xri:@openmdx:org.opencrx.kernel.account1/provider/" + providerName + "/segment/" + segmentName + "/account/" + accountQualifier)
             );
    			ObjectReference accountRef = new ObjectReference(account, app);
    			String accountTitle = app.getHtmlEncoder().encode(accountRef.getTitle(), false);
    			String test = account.refMofId();
    			int plusLocation = test.indexOf("+");
    			while (plusLocation >= 0) {
    				test = test.substring(0, plusLocation) + "%2B" + test.substring(plusLocation + 1);
    				plusLocation = test.indexOf("+");
    			}
    			String encodedAccountXri = test;
%>
          <li><a href="../../<%= accountRef.getSelectObjectAction().getEncodedHRef(requestId) %>">* </a><a href="<%= wizardRelationshipGraph %>?xri=<%= encodedAccountXri + "&" + requestIdParam + "&" + xriParam %>" target="_blank"><%= accountTitle %></a></li>
<%
        }

%>
        </ul>
<%
      }
%>
    </div>
<%
  }
%>

</form>
<%
  }
  catch (Exception ex) {
      ServiceException e0 = new ServiceException(ex);
      e0.log();
      out.println("<p><b>!! Failed !!<br><br>The following exception(s) occured:</b><br><br><pre>");
      PrintWriter pw = new PrintWriter(out);
      e0.printStackTrace(pw);
      out.println("</pre></p>");
  } finally {
	  if(pm != null) {
		  pm.close();
	  }
  }
%>
      </div> <!-- content -->
    </div> <!-- content-wrap -->
  </div> <!-- wrap -->
</div> <!-- container -->
</body>
</html>
