<%@  page contentType="text/html;charset=UTF-8" language="java" pageEncoding="UTF-8" %><%
/*
 * ====================================================================
 * Project:     openCRX/Core, http://www.opencrx.org/
 * Description: create Bulk E-mail (e.g. for campaign)
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
java.io.*,
java.text.*,
org.openmdx.kernel.id.cci.*,
org.openmdx.base.accessor.jmi.cci.*,
org.openmdx.base.exception.*,
org.openmdx.portal.servlet.*,
org.openmdx.portal.servlet.attribute.*,
org.openmdx.portal.servlet.component.*,
org.openmdx.portal.servlet.control.*,
org.openmdx.portal.servlet.action.*,
org.openmdx.portal.servlet.wizards.*,
org.openmdx.base.naming.*,
org.openmdx.kernel.log.*,
org.openmdx.kernel.id.*,
org.openmdx.uses.org.apache.commons.fileupload.*
" %><%
  request.setCharacterEncoding("UTF-8");
	ApplicationContext app = (ApplicationContext)session.getValue(WebKeys.APPLICATION_KEY);

  boolean uploadFailed = false;
  String location = null; // newly uploaed file

	Map parameterMap = request.getParameterMap();
  if(FileUpload.isMultipartContent(request)) {
		parameterMap = new HashMap();
  	DiskFileUpload upload = new DiskFileUpload();
  	upload.setHeaderEncoding("UTF-8");
  	List items = null;
    try {
      items = upload.parseRequest(
        request,
        200, // in-memory threshold. Content for fields larger than threshold is written to disk
	      50000000, // max request size [overall limit]
        app.getTempDirectory().getPath()
      );
		}
		catch(FileUploadException e) {
		  uploadFailed = true;
			SysLog.warning("cannot upload file", e.getMessage());
%>
      <div style="padding:10px 10px 10px 10px;background-color:#FF0000;color:#FFFFFF;">
        <table>
          <tr>
            <td style="padding:5px;"><b>ERROR</b>:</td>
            <td>cannot upload file - <%= e.getMessage() %></td>
          </tr>
        </table>
      </div>
<%
		}
		try {
      if (uploadFailed) {
        items = upload.parseRequest(
          request,
          200, // in-memory threshold. Content for fields larger than threshold is written to disk
          60000000, // max request size [overall limit]
          app.getTempDirectory().getPath()
        );
      }
      for(Iterator i = items.iterator(); i.hasNext(); ) {
        FileItem item = (FileItem)i.next();
        if(item.isFormField()) {
          parameterMap.put(
            item.getFieldName(),
            new String[]{item.getString("UTF-8")}
          );
        }
        else {
          // reset binary
          if("#NULL".equals(item.getName())) {
            parameterMap.put(
              item.getFieldName(),
              new String[]{item.getName()}
            );
          }
          // add to parameter map if file received
          else if(item.getSize() > 0) {
            parameterMap.put(
              item.getFieldName(),
              new String[]{item.getName()}
            );
            location = app.getTempFileName(org.opencrx.kernel.backend.Activities.getInstance().getUidAsString(), "");

            // bytes
            File outFile = new File(location);
            item.write(outFile);

            // type
            PrintWriter pw = new PrintWriter(
              new FileOutputStream(location + ".INFO")
            );
            pw.println(item.getContentType());
            int sep = item.getName().lastIndexOf("/");
            if(sep < 0) {
              sep = item.getName().lastIndexOf("\\");
            }
            pw.println(item.getName().substring(sep + 1));
            pw.close();

          }
        }
      }
		}
		catch(FileUploadException e) {
			SysLog.warning("cannot upload file", e.getMessage());
%>
      <div style="padding:10px 10px 10px 10px;background-color:#FF0000;color:#FFFFFF;">
        <table>
          <tr>
            <td style="padding:5px;"><b>ERROR</b>:</td>
            <td>cannot upload file - <%= e.getMessage() %></td>
          </tr>
        </table>
      </div>
<%
		}
  }

	String[] requestIds = (String[])parameterMap.get(Action.PARAMETER_REQUEST_ID);
	String requestId = (requestIds == null) || (requestIds.length == 0) ? request.getParameter(Action.PARAMETER_REQUEST_ID) : requestIds[0];
	String requestIdParam = Action.PARAMETER_REQUEST_ID + "=" + requestId;
	ViewsCache viewsCache = (ViewsCache)session.getValue(WebKeys.VIEW_CACHE_KEY_SHOW);
	String[] objectXris = (String[])parameterMap.get("xri");
	String objectXri = (objectXris == null) || (objectXris.length == 0) ? null : objectXris[0];
	if(app == null || objectXri == null || viewsCache.getView(requestId) == null) {
    response.sendRedirect(
       request.getContextPath() + "/" + WebKeys.SERVLET_NAME
    );
    return;
  }
  javax.jdo.PersistenceManager pm = app.getNewPmData();
  Texts_1_0 texts = app.getTexts();

%>
<!DOCTYPE HTML PUBLIC "-//W3C//DTD HTML 4.01 Transitional//EN" "http://www.w3.org/TR/html4/loose.dtd">

<html>

<head>
  <title>Bulk E-Mail</title>
  <meta http-equiv="Content-Type" content="text/html; charset=utf-8">
  <meta http-equiv="Content-Type" content="text/html; charset=utf-8">
  <link href="../../_style/n2default.css" rel="stylesheet" type="text/css">
  <link href="../../_style/colors.css" rel="stylesheet" type="text/css">
  <link href="../../_style/ssf.css" rel="stylesheet" type="text/css">
	<script type="text/javascript" src="../../js/portal-all.js"></script>
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
  <style type="text/css" media="all">
    html,body {margin:0;border:0;padding:1;background:white;}
    fieldset {background-color:#F2F2F2;margin:15px 0 15px 0;}
    .fieldGroup td {padding:2px;}
    legend span {font-size:14px;vertical-align:baseline;}
    legend INPUT {font-size:10px;}
    input.lightUp:focus, textarea.lightUp:focus, select.lightUp:focus, input.lightUp.sffocus, textarea.lightUp.sffocus, select.lightUp.sffocus {
    	background-color: #ff8;
    	border-color: black;
    }
    input.lightUp:hover, textarea.lightUp:hover, select.lightUp:hover, input.lightUp.sfhover, textarea.lightUp.sfhover, select.lightUp.sfhover {
    	background-color: #ff8;
    }
    input.lightUp, textarea.lightUp, select.lightUp {
    	background-color: #dfd;
    }
  </style>
  <link rel='shortcut icon' href='../../images/favicon.ico' />
</head>
<body>
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
    	<div id="content" style="padding:100px 0.5em 0px 0.5em;">
<%

        final String FORM_ACTION = "BulkEmail.jsp";
        final String ACTIVITYCREATOR_CLASS = "org:opencrx:kernel:activity1:ActivityCreator";
        final String ADDRESSGROUP_CLASS = "org:opencrx:kernel:activity1:AddressGroup";
        final String EMAIL_CLASS = "org:opencrx:kernel:activity1:EMail";
        final String EMAILADDRESS_CLASS = "org:opencrx:kernel:account1:EMailAddress";
        final String featurePartyTypeEMailRecipient = "partytypeAbstractActivityParty";
        final Short CODE_FROM = 210;
        final Short CODE_TO = 220;
        final Short CODE_BCC = 240;
        final Short ACTIVITY_STATE_CLOSED = 20;
        final Short MAX_FILES = 3;

        final String bgColorError = "#F77747";

        java.util.Date today = new java.util.Date();

      	try {
          Codes codes = app.getCodes();
          Map<String,Short> featurePartyTypeEMailRecipient_longTextsT = codes.getLongTextByText(featurePartyTypeEMailRecipient, app.getCurrentLocaleAsIndex(), true);

          // Timezone is reusable
          final TimeZone utc = TimeZone.getTimeZone(app.getCurrentTimeZone());
          // DateFormat is not multi-thread-safe!
          DateFormat dateFormat = new SimpleDateFormat("yyyyMMdd");
          dateFormat.setLenient(false); // if the timestamp string is always complete
          dateFormat.setTimeZone(utc);
          DateFormat crxDateFormat = new SimpleDateFormat("yyyy-MM-dd'T'HH:mm:ss.SSS'Z'");
          crxDateFormat.setLenient(false); // if the timestamp string is always complete
          crxDateFormat.setTimeZone(utc);

          NumberFormat formatter = new DecimalFormat("0");
          NumberFormat lzformatter = new DecimalFormat("00");
          NumberFormat amountFormatter = new DecimalFormat("#,##0.00");

          Path objectPath = new Path(objectXri);
          RefObject_1_0 obj = (RefObject_1_0)pm.getObjectById(objectPath);
          String providerName = objectPath.get(2);
          String segmentName = objectPath.get(4);

      	  boolean actionOk = parameterMap.get("Ok.Button") != null;
      	  boolean actionCancel = parameterMap.get("Cancel.Button") != null;
          if (actionCancel) {
      	    // Go back to previous view
            Action nextAction = new ObjectReference(
              (RefObject_1_0)pm.getObjectById(new Path(objectXri)),
              app
            ).getSelectObjectAction();
        		response.sendRedirect(
        			request.getContextPath() + "/" + nextAction.getEncodedHRef()
          	);
          }

          // Get activity segment
          org.opencrx.kernel.activity1.jmi1.Segment activitySegment =
            (org.opencrx.kernel.activity1.jmi1.Segment)pm.getObjectById(
              new Path("xri:@openmdx:org.opencrx.kernel.activity1/provider/" + providerName + "/segment/" + segmentName)
             );

          // Get account1 and activity1 package
          UserDefinedView userView = new UserDefinedView(
            (RefObject_1_0)pm.getObjectById(new Path(objectXri)),
            app,
            viewsCache.getView(requestId)
          );

          org.opencrx.kernel.activity1.jmi1.ActivityCreator currentActivityCreator = null;

          String[] mpparams = (String[])parameterMap.get("activityCreatorSelector");
          String activityCreatorSelector = (mpparams == null) || (mpparams.length == 0) ? null : mpparams[0];
          if ((activityCreatorSelector == null) || (activityCreatorSelector.length() < 2)) {
            activityCreatorSelector = null;
          } else {
            currentActivityCreator = (org.opencrx.kernel.activity1.jmi1.ActivityCreator)pm.getObjectById(new Path(activityCreatorSelector));
          }

          // FROM
          boolean senderOk = false;
          org.opencrx.kernel.account1.jmi1.EMailAddress senderEmail = null;
        	mpparams = (String[])parameterMap.get("senderEmailXri");
        	String senderEmailXri = (mpparams == null) || (mpparams.length == 0) ? null : mpparams[0];
          if ((senderEmailXri != null) && (senderEmailXri.length() > 0)) {
            try {
              senderEmail = (org.opencrx.kernel.account1.jmi1.EMailAddress)pm.getObjectById(new Path(senderEmailXri));
              senderOk = (senderEmail != null) && (senderEmail.getEmailAddress() != null);
            } catch (Exception e) {
              senderEmailXri = null;
            };
          } else {
            senderEmailXri = null;
          }

          // TO
          boolean recipientOk = false;
          org.opencrx.kernel.account1.jmi1.EMailAddress recipientEmail = null;
        	mpparams = (String[])parameterMap.get("recipientEmailXri");
        	String recipientEmailXri = (mpparams == null) || (mpparams.length == 0) ? null : mpparams[0];
          if ((recipientEmailXri != null) && (recipientEmailXri.length() > 0)) {
            try {
              recipientEmail = (org.opencrx.kernel.account1.jmi1.EMailAddress)pm.getObjectById(new Path(recipientEmailXri));
              recipientOk = (recipientEmail != null) && (recipientEmail.getEmailAddress() != null);
            } catch (Exception e) {
              recipientEmailXri = null;
            };
          } else {
            recipientEmailXri = null;
          }

          // BCC AddressGroup
          boolean bulkRecipientGroupOk = false;
          org.opencrx.kernel.activity1.jmi1.AddressGroup bulkRecipientGroup = null;
        	mpparams = (String[])parameterMap.get("bulkRecipientGroupXri");
        	String bulkRecipientGroupXri = (mpparams == null) || (mpparams.length == 0) ? null : mpparams[0];
          if ((bulkRecipientGroupXri != null) && (bulkRecipientGroupXri.length() > 0)) {
            try {
              bulkRecipientGroup = (org.opencrx.kernel.activity1.jmi1.AddressGroup)pm.getObjectById(new Path(bulkRecipientGroupXri));
              bulkRecipientGroupOk = bulkRecipientGroup != null;
            } catch (Exception e) {
              bulkRecipientGroupXri = null;
            };
          } else {
            bulkRecipientGroupXri = null;
          }

          // Gateway
          org.opencrx.kernel.account1.jmi1.EMailAddress gatewayEmail = null;
        	mpparams = (String[])parameterMap.get("gatewayEmailXri");
        	String gatewayEmailXri = (mpparams == null) || (mpparams.length == 0) ? null : mpparams[0];
          if ((gatewayEmailXri != null) && (gatewayEmailXri.length() > 0)) {
            try {
              gatewayEmail = (org.opencrx.kernel.account1.jmi1.EMailAddress)pm.getObjectById(new Path(gatewayEmailXri));
              senderOk = (gatewayEmail != null) && (gatewayEmail.getEmailAddress() != null);
            } catch (Exception e) {
              gatewayEmailXri = null;
            };
          } else {
            gatewayEmailXri = null;
          }

        	mpparams = (String[])parameterMap.get("messageSubject");
        	String messageSubject = (mpparams == null) || (mpparams.length == 0) ? " - ${#}" : mpparams[0];

        	mpparams = (String[])parameterMap.get("messageBody");
        	String messageBody = (mpparams == null) || (mpparams.length == 0) ? "" : mpparams[0];

        	mpparams = (String[])parameterMap.get("activityName");
        	String activityName = (mpparams == null) || (mpparams.length == 0) ? "" : mpparams[0];

        	mpparams = (String[])parameterMap.get("activityDescription");
        	String activityDescription = (mpparams == null) || (mpparams.length == 0) ? "" : mpparams[0];

          Set filenames = new HashSet();
          // get names of previously available files
          for (int i = 0; i < MAX_FILES; i++) {
            try {
            	mpparams = (String[])parameterMap.get("filename" + Integer.toString(i));
            	if ((mpparams != null) && (mpparams.length >= 0)) {
            	  filenames.add(mpparams[0]);
                //System.out.println("adding existing location="+(String)mpparams[0]);
            	}
            } catch(Exception e) {}
          }

          // add newly uploaded file
          if (location != null) {
        	  filenames.add(location);
            //System.out.println("adding new filename="+location);
          }

          boolean commitAndSend = parameterMap.get("commitAndSend.CheckBox") != null;

          boolean sendOk = senderOk && recipientOk && bulkRecipientGroupOk &&
                           (currentActivityCreator != null);
          if (actionOk && sendOk) {
            // create EMailActivity

            org.opencrx.kernel.account1.jmi1.Contact contact = null;
            // get UserHome
            org.opencrx.kernel.home1.jmi1.UserHome userHome = org.opencrx.kernel.backend.UserHomes.getInstance().getUserHome(obj.refGetPath(), pm);
            contact = userHome.getContact();

            // create Email Activity
            pm.currentTransaction().begin();
            //System.out.println("creating new Activity");
            org.opencrx.kernel.activity1.jmi1.NewActivityParams params = org.w3c.spi2.Structures.create(
           		org.opencrx.kernel.activity1.jmi1.NewActivityParams.class, 
   				org.w3c.spi2.Datatypes.member(org.opencrx.kernel.activity1.jmi1.NewActivityParams.Member.creationContext, null),
   				org.w3c.spi2.Datatypes.member(org.opencrx.kernel.activity1.jmi1.NewActivityParams.Member.description, activityDescription),			
   				org.w3c.spi2.Datatypes.member(org.opencrx.kernel.activity1.jmi1.NewActivityParams.Member.detailedDescription, null),			
   				org.w3c.spi2.Datatypes.member(org.opencrx.kernel.activity1.jmi1.NewActivityParams.Member.dueBy, today),			
   				org.w3c.spi2.Datatypes.member(org.opencrx.kernel.activity1.jmi1.NewActivityParams.Member.icalType, (short)0),	
   				org.w3c.spi2.Datatypes.member(org.opencrx.kernel.activity1.jmi1.NewActivityParams.Member.name, activityName),	
   				org.w3c.spi2.Datatypes.member(org.opencrx.kernel.activity1.jmi1.NewActivityParams.Member.priority, (short)2),
   				org.w3c.spi2.Datatypes.member(org.opencrx.kernel.activity1.jmi1.NewActivityParams.Member.reportingContact, contact),
   				org.w3c.spi2.Datatypes.member(org.opencrx.kernel.activity1.jmi1.NewActivityParams.Member.scheduledEnd, today),
   				org.w3c.spi2.Datatypes.member(org.opencrx.kernel.activity1.jmi1.NewActivityParams.Member.scheduledStart, today)
   			); 
            org.opencrx.kernel.activity1.jmi1.NewActivityResult result = currentActivityCreator.newActivity(params);
            pm.currentTransaction().commit();

            pm.currentTransaction().begin();
            org.opencrx.kernel.activity1.jmi1.EMail newActivity  = (org.opencrx.kernel.activity1.jmi1.EMail)pm.getObjectById(result.getActivity().refGetPath());

            newActivity.setMessageSubject(messageSubject.replace("${#}", "#" + newActivity.getActivityNumber()));
            newActivity.setMessageBody(messageBody);
            newActivity.setAssignedTo(contact);
            if (commitAndSend) {
              newActivity.setActivityState((short)ACTIVITY_STATE_CLOSED);
            }

            if (senderEmail != null) {
              newActivity.setSender(senderEmail);
            }

            if (gatewayEmail != null) {
              newActivity.setGateway(gatewayEmail);
            }

            if (recipientEmail != null) {
              org.opencrx.kernel.activity1.jmi1.EMailRecipient emailRecipient = pm.newInstance( org.opencrx.kernel.activity1.jmi1.EMailRecipient.class);
              emailRecipient.setParty(recipientEmail);
              emailRecipient.setPartyType((short)CODE_TO); // TO
              newActivity.addEmailRecipient(
                org.opencrx.kernel.backend.Base.getInstance().getUidAsString(),
                emailRecipient
              );
            }

            if (bulkRecipientGroup != null) {
              org.opencrx.kernel.activity1.jmi1.EMailRecipientGroup emailRecipientGroup = pm.newInstance(org.opencrx.kernel.activity1.jmi1.EMailRecipientGroup.class);
              emailRecipientGroup.setParty(bulkRecipientGroup);
              emailRecipientGroup.setPartyType((short)CODE_BCC); // BCC
              newActivity.addEmailRecipient(
                org.opencrx.kernel.backend.Base.getInstance().getUidAsString(),
                emailRecipientGroup
              );
            }
            int m = 0;
            for (Iterator i = filenames.iterator(); i.hasNext();) {
              try {
                String filename = (String)i.next();
                // mimeType and name
                BufferedReader r = new BufferedReader(
                  new FileReader(filename + ".INFO")
                );
                String contentMimeType = r.readLine();
                String contentName = r.readLine();
                r.close();

        				org.opencrx.kernel.generic.jmi1.Media media = pm.newInstance(org.opencrx.kernel.generic.jmi1.Media.class);
        				media.setContentName(contentName);
        				media.setContentMimeType(contentMimeType);
      		    	media.setContent(
                  org.w3c.cci2.BinaryLargeObjects.valueOf(new File(filename))
                );
        				newActivity.addMedia(
        					org.opencrx.kernel.backend.Base.getInstance().getUidAsString(),
        					media
        				);
        		  } catch(Exception e) {}

              m++;

            }

            pm.currentTransaction().commit();

            if (commitAndSend) {
              // get Workflow
              org.opencrx.kernel.workflow1.jmi1.WfProcess workflow =
                (org.opencrx.kernel.workflow1.jmi1.WfProcess)pm.getObjectById(
                  new Path("xri:@openmdx:org.opencrx.kernel.workflow1/provider/" + providerName + "/segment/" + segmentName + "/wfProcess/SendMail")
                );

              // create Workflow params
              // Get base package
              org.opencrx.kernel.base.jmi1.ExecuteWorkflowParams wfparams = org.w3c.spi2.Structures.create(
                org.opencrx.kernel.base.jmi1.ExecuteWorkflowParams.class, 
     			org.w3c.spi2.Datatypes.member(org.opencrx.kernel.base.jmi1.ExecuteWorkflowParams.Member.name, "BulkEmail"),
     			org.w3c.spi2.Datatypes.member(org.opencrx.kernel.base.jmi1.ExecuteWorkflowParams.Member.parentProcessInstance, null),			
     			org.w3c.spi2.Datatypes.member(org.opencrx.kernel.base.jmi1.ExecuteWorkflowParams.Member.targetObject, (org.openmdx.base.jmi1.BasicObject) newActivity),			
     			org.w3c.spi2.Datatypes.member(org.opencrx.kernel.base.jmi1.ExecuteWorkflowParams.Member.triggeredBy, null),	
     			org.w3c.spi2.Datatypes.member(org.opencrx.kernel.base.jmi1.ExecuteWorkflowParams.Member.triggeredByEventId, null),	
     			org.w3c.spi2.Datatypes.member(org.opencrx.kernel.base.jmi1.ExecuteWorkflowParams.Member.triggeredByEventType, null),
     			org.w3c.spi2.Datatypes.member(org.opencrx.kernel.base.jmi1.ExecuteWorkflowParams.Member.workflow, workflow)
			  ); 
              try {
                  pm.currentTransaction().begin();
                  org.opencrx.kernel.base.jmi1.ExecuteWorkflowResult wfresult = userHome.executeWorkflow(wfparams);
                  pm.currentTransaction().commit();

                  org.opencrx.kernel.home1.jmi1.WfProcessInstance wfProcessInstance =
                    (org.opencrx.kernel.home1.jmi1.WfProcessInstance)pm.getObjectById(wfresult.getWorkflowInstance().refGetPath());
                  pm.currentTransaction().begin();
                  newActivity.setDetailedDescription("SendMail process instance: \n" + wfProcessInstance.refMofId());
                  pm.currentTransaction().commit();
              }
              catch(Exception e) {
                  try {
                      pm.currentTransaction().rollback();
                  } catch(Exception e1) {}
			      ServiceException e0 = new ServiceException(e);
			      e0.log();
			      out.println("<p><b>!! Failed !!<br><br>The following exception(s) occured:</b><br><br><pre>");
			      PrintWriter pw = new PrintWriter(out);
			      e0.printStackTrace(pw);
			      out.println("</pre></p>");
              }
            }

            try {
              // navigate to created Activity
              Action nextAction =
                new Action(
            	  SelectObjectAction.EVENT_ID,
                  new Action.Parameter[]{
                    new Action.Parameter(Action.PARAMETER_OBJECTXRI, newActivity.refMofId())
                    },
                  "", true
        		  );
              response.sendRedirect(
                 request.getContextPath() + "/" + nextAction.getEncodedHRef()
              );

            } catch(Exception e) {}
          } else {
%>
            <div id="etitle" style="height:20px;">
              Bulk E-mail
            </div>
            <form name="bulkEmail" accept-charset="utf-8" enctype="multipart/form-data" method="post" action="<%= "../.." + request.getServletPath() %>">
              <input type="hidden" name="xri" value="<%= objectXri %>" />
              <input type="hidden" name="<%= Action.PARAMETER_REQUEST_ID %>" value="<%= requestId %>" />

            	<br>

            	<fieldset>
            		<legend><span><%= app.getLabel(ACTIVITYCREATOR_CLASS) %></span></legend>
                <table class="fieldGroup">
                  <tr>
                    <td class="<%= CssClass.fieldLabel %>"><span class="nw"><%= userView.getFieldLabel(ACTIVITYCREATOR_CLASS, "name", app.getCurrentLocaleAsIndex()) %>:</span></td>
                    <td>
                      <select class="valueL" name="activityCreatorSelector" tabindex="10">
                        <option value="0">N/A</option>
<%
                        // get ActivityCreators sorted by name (asc) for SalesRep responsible for customer
                        org.opencrx.kernel.activity1.cci2.ActivityCreatorQuery activityCreatorFilter = (org.opencrx.kernel.activity1.cci2.ActivityCreatorQuery)pm.newQuery(org.opencrx.kernel.activity1.jmi1.ActivityCreator.class);
                        activityCreatorFilter.orderByName().ascending();
                        int maxCreator = 200;
                        int counter = 0;

                        for (
                          Iterator j = activitySegment.getActivityCreator(activityCreatorFilter).iterator();
                          j.hasNext() && counter < maxCreator;
                        ) {

                          String selectedModifier = "";
                          // get ActivityCreator
                    	  org.opencrx.kernel.activity1.jmi1.ActivityCreator activityCreator =
                            (org.opencrx.kernel.activity1.jmi1.ActivityCreator)j.next();

                          if (
                              (activityCreator.getActivityType() != null) &&
                              (((org.opencrx.kernel.activity1.jmi1.ActivityType)activityCreator.getActivityType()).getActivityClass() == (short)0)
                              /* 0 = E-mail
                                 1 = DEPRECATED (was Fax)
                                 2 = Incident
                                 3 = Mailing
                                 4 = Meeting
                                 5 = DEPRECATED (was MMS)
                                 6 = Phone Call
                                 7 = DEPRECATED (was SMS)
                                 8 = Task
                                 9 = Absense
                                10 = External Activity
                                11 = Sales Visit
                              */
                          ) {
                            counter++;
                            selectedModifier = (activityCreatorSelector != null) && (activityCreatorSelector.compareTo((activityCreator.refMofId()).toString()) == 0) ? "selected" : "";
%>
                            <option <%= selectedModifier %> value="<%= activityCreator.refMofId() %>"><%= activityCreator.getName() != null ? activityCreator.getName() : "Kein Name" %><%= activityCreator.getDescription() != null ? " / " + activityCreator.getDescription() : "" %></option>
<%
                          }
                        }
%>
                      </select>
        	          </td>
         	          <td class="addon">&nbsp;</td>
         	        </tr>
         	      </table>
       	      </fieldset>

            	<fieldset>
            		<legend><span><img border="0" src="../../images/EMail.gif" alt="E-mail" /></span></legend>
                <table class="fieldGroup">
                  <tr>
                    <td class="<%= CssClass.fieldLabel %>"><span class="nw"><%= userView.getFieldLabel(EMAIL_CLASS, "name", app.getCurrentLocaleAsIndex()) %>:</span></td>
                    <td>
                      <input type="text" id="activityName" name="activityName" class="valueL" tabindex="20" value="<%= activityName %>" />
                    </td>
                    <td class="addon" ></td>
                  </tr>
                  <tr>
                    <td class="<%= CssClass.fieldLabel %>"><span class="nw"><%= userView.getFieldLabel(EMAIL_CLASS, "description", app.getCurrentLocaleAsIndex()) %>:</span></td>
                    <td>
                      <input type="text" id="activityDescription" name="activityDescription" class="valueL" tabindex="30" value="<%= activityDescription %>" />
                    </td>
                    <td class="addon" ></td>
                  </tr>
                  <tr>
                    <td>&nbsp;</td>
                    <td></td>
                  </tr>
                  <tr>
                    <td class="<%= CssClass.fieldLabel %>"><span class="nw"><%= (codes.getLongTextByCode(featurePartyTypeEMailRecipient, app.getCurrentLocaleAsIndex(), true).get(CODE_FROM)) %>:</span></td>
                    <td>
                      <div class="autocompleterMenu">
                        <ul id="<%=CssClass.ssf_nav %>" class="<%=CssClass.ssf_nav %>" onmouseover="sfinit(this);" >
                          <li><a href="#"><img border="0" alt="" src="../../images/autocomplete_select.png" /></a>
                            <ul onclick="this.style.left='-999em';" onmouseout="this.style.left='';">
                              <li class="selected"><a href="#" onclick="javascript:navSelect(this);ac10.url= getEncodedHRef(['../../ObjectInspectorServlet', 'event', '40', 'parameter', 'xri*(xri:@openmdx:org.opencrx.kernel.account1/provider/<%=providerName%>/segment/<%=segmentName%>)*referenceName*(address)*filterByType*(org:opencrx:kernel:account1:AccountAddress)*filterByFeature*(emailAddress)*filterOperator*(IS_LIKE)*orderByFeature*(emailAddress)*position*(0)*size*(20)']);return false;"><span>&nbsp;&nbsp;&nbsp;</span><%=app.getLabel(EMAILADDRESS_CLASS)%></a></li>
                            </ul>
                          </li>
                        </ul>
                      </div>
                      <div class="autocompleterInput">
<%
						String senderEmailLookupId = org.opencrx.kernel.backend.Activities.getInstance().getUidAsString();
                        Action findEmailAddressObjectAction = Action.getFindObjectAction("org:opencrx:kernel:account1:Segment:address", senderEmailLookupId);
%>
                        <input type="text" class="valueL valueAC" id="senderEmailXri.Title" name="senderEmailXri.Title" tabindex="40" <%=!senderOk ? "style='background-color:" + bgColorError + ";'" : ""%> value="<%=(senderEmail != null) && (senderEmail.getEmailAddress() != null) ? senderEmail.getEmailAddress() : ""%>" onblur="javascript:this.style.backgroundColor='';" />
                      </div>
                      <input type="hidden" class="valueLLocked" id="senderEmailXri" name="senderEmailXri" readonly value="<%=senderEmailXri != null ? senderEmailXri : ""%>" />
                      <div class="autocomplete" id="senderEmailXri.Update" style="display:none;z-index:500;"></div>
                      <script type="text/javascript" language="javascript" charset="utf-8">
                        ac10 = new Ajax.Autocompleter(
                          'senderEmailXri.Title',
                          'senderEmailXri.Update',
                          '../../ObjectInspectorServlet?event=40&parameter=xri*%28xri%3A%40openmdx%3Aorg.opencrx.kernel.account1%2Fprovider%2F<%=providerName%>%2Fsegment%2F<%=segmentName%>%29*referenceName*%28address%29*filterByType*%28org:opencrx:kernel:account1:AccountAddress%29*filterByFeature*%28emailAddress%29*filterOperator*%28IS_LIKE%29*orderByFeature*%28emailAddress%29*position*%280%29*size*%2820%29',
                          {
                            paramName: 'filtervalues',
                            minChars: 2,
                            afterUpdateElement: updateXriField
                          }
                        );
                      </script>
                    </td>
                    <td class="addon">
                      <img class="popUpButton" border="1" alt="lookup" title="" src="../../images/<%=findEmailAddressObjectAction.getIconKey()%>" onclick="OF.findObject('../../<%=findEmailAddressObjectAction.getEncodedHRef(requestId)%>', document.forms['bulkEmail'].elements['senderEmailXri.Title'], document.forms['bulkEmail'].elements['senderEmailXri'], '<%=senderEmailLookupId%>');" />
                    </td>
                  </tr>

                  <tr>
                    <td class="<%= CssClass.fieldLabel %>"><span class="nw"><%=(codes.getLongTextByCode(featurePartyTypeEMailRecipient, app.getCurrentLocaleAsIndex(), true).get(CODE_TO))%>:</span></td>
                    <td>
                      <div class="autocompleterMenu">
                        <ul id="<%=CssClass.ssf_nav%>" class="<%=CssClass.ssf_nav%>" onmouseover="sfinit(this);" >
                          <li><a href="#"><img border="0" alt="" src="../../images/autocomplete_select.png" /></a>
                            <ul onclick="this.style.left='-999em';" onmouseout="this.style.left='';">
                              <li class="selected"><a href="#" onclick="javascript:navSelect(this);ac20.url= getEncodedHRef(['../../ObjectInspectorServlet', 'event', '40', 'parameter', 'xri*(xri:@openmdx:org.opencrx.kernel.account1/provider/<%=providerName%>/segment/<%=segmentName%>)*referenceName*(address)*filterByType*(org:opencrx:kernel:account1:AccountAddress)*filterByFeature*(emailAddress)*filterOperator*(IS_LIKE)*orderByFeature*(emailAddress)*position*(0)*size*(20)']);return false;"><span>&nbsp;&nbsp;&nbsp;</span><%=app.getLabel(EMAILADDRESS_CLASS)%></a></li>
                            </ul>
                          </li>
                        </ul>
                      </div>
                      <div class="autocompleterInput">
<%
						String recipientEmailLookupId = org.opencrx.kernel.backend.Activities.getInstance().getUidAsString();
                        findEmailAddressObjectAction = Action.getFindObjectAction("org:opencrx:kernel:account1:Segment:address", recipientEmailLookupId);
%>
                        <input type="text" class="valueL valueAC" id="recipientEmailXri.Title" name="recipientEmailXri.Title" tabindex="50" <%=!recipientOk ? "style='background-color:" + bgColorError + ";'" : ""%> value="<%=(recipientEmail != null) && (recipientEmail.getEmailAddress() != null) ? recipientEmail.getEmailAddress() : ""%>" onblur="javascript:this.style.backgroundColor='';" />
                      </div>
                      <input type="hidden" class="valueLLocked" id="recipientEmailXri" name="recipientEmailXri" readonly value="<%=recipientEmailXri != null ? recipientEmailXri : ""%>" />
                      <div class="autocomplete" id="recipientEmailXri.Update" style="display:none;z-index:500;"></div>
                      <script type="text/javascript" language="javascript" charset="utf-8">
                        ac20 = new Ajax.Autocompleter(
                          'recipientEmailXri.Title',
                          'recipientEmailXri.Update',
                          '../../ObjectInspectorServlet?event=40&parameter=xri*%28xri%3A%40openmdx%3Aorg.opencrx.kernel.account1%2Fprovider%2F<%=providerName%>%2Fsegment%2F<%=segmentName%>%29*referenceName*%28address%29*filterByType*%28org:opencrx:kernel:account1:AccountAddress%29*filterByFeature*%28emailAddress%29*filterOperator*%28IS_LIKE%29*orderByFeature*%28emailAddress%29*position*%280%29*size*%2820%29',
                          {
                            paramName: 'filtervalues',
                            minChars: 2,
                            afterUpdateElement: updateXriField
                          }
                        );
                      </script>
                    </td>
                    <td class="addon">
                      <img class="popUpButton" border="1" alt="lookup" title="" src="../../images/<%=findEmailAddressObjectAction.getIconKey()%>" onclick="OF.findObject('../../<%=findEmailAddressObjectAction.getEncodedHRef(requestId)%>', document.forms['bulkEmail'].elements['recipientEmailXri.Title'], document.forms['bulkEmail'].elements['recipientEmailXri'], '<%=recipientEmailLookupId%>');" />
                    </td>
                  </tr>

                  <tr>
                    <td class="<%= CssClass.fieldLabel %>"><span class="nw"><%=(codes.getLongTextByCode(featurePartyTypeEMailRecipient, app.getCurrentLocaleAsIndex(), true).get(CODE_BCC))%> (<%=app.getLabel(ADDRESSGROUP_CLASS)%>):</span></td>
                    <td>
                      <div class="autocompleterMenu">
                        <ul id="<%=CssClass.ssf_nav%>" class="<%=CssClass.ssf_nav%>" onmouseover="sfinit(this);" >
                          <li><a href="#"><img border="0" alt="" src="../../images/autocomplete_select.png" /></a>
                            <ul onclick="this.style.left='-999em';" onmouseout="this.style.left='';">
                              <li class="selected"><a href="#" onclick="javascript:navSelect(this);ac30.url= getEncodedHRef(['../../ObjectInspectorServlet', 'event', '40', 'parameter', 'xri*(xri:@openmdx:org.opencrx.kernel.activity1/provider/<%=providerName%>/segment/<%=segmentName%>)*referenceName*(addressGroup)*filterByFeature*(name)*filterOperator*(IS_LIKE)*orderByFeature*(name)*position*(0)*size*(20)']);return false;"><span>&nbsp;&nbsp;&nbsp;</span><%=userView.getFieldLabel(ADDRESSGROUP_CLASS, "name", app.getCurrentLocaleAsIndex())%></a></li>
                              <li><a href="#" onclick="javascript:navSelect(this);ac30.url= getEncodedHRef(['../../ObjectInspectorServlet', 'event', '40', 'parameter', 'xri*(xri:@openmdx:org.opencrx.kernel.activity1/provider/<%=providerName%>/segment/<%=segmentName%>)*referenceName*(addressGroup)*filterByFeature*(description)*filterOperator*(IS_LIKE)*orderByFeature*(description)*position*(0)*size*(20)']);return false;"><span>&nbsp;&nbsp;&nbsp;</span><%=userView.getFieldLabel(ADDRESSGROUP_CLASS, "description", app.getCurrentLocaleAsIndex())%></a></li>
                            </ul>
                          </li>
                        </ul>
                      </div>
                      <div class="autocompleterInput">
<%
						String addressGroupLookupId = org.opencrx.kernel.backend.Activities.getInstance().getUidAsString();
                        Action findAddressGroupObjectAction = Action.getFindObjectAction("org:opencrx:kernel:activity1:Segment:addressGroup", addressGroupLookupId);
%>
                        <input type="text" class="valueL valueAC" id="bulkRecipientGroupXri.Title" name="bulkRecipientGroupXri.Title" tabindex="60" <%=!bulkRecipientGroupOk ? "style='background-color:" + bgColorError + ";'" : ""%> value="<%=(bulkRecipientGroup != null) && (bulkRecipientGroup.getName() != null) ? bulkRecipientGroup.getName() : ""%>" onblur="javascript:this.style.backgroundColor='';" />
                      </div>
                      <input type="hidden" class="valueLLocked" id="bulkRecipientGroupXri" name="bulkRecipientGroupXri" readonly value="<%=bulkRecipientGroupXri != null ? bulkRecipientGroupXri : ""%>" />
                      <div class="autocomplete" id="bulkRecipientGroupXri.Update" style="display:none;z-index:500;"></div>
                      <script type="text/javascript" language="javascript" charset="utf-8">
                        ac30 = new Ajax.Autocompleter(
                          'bulkRecipientGroupXri.Title',
                          'bulkRecipientGroupXri.Update',
                          '../../ObjectInspectorServlet?event=40&parameter=xri*%28xri%3A%40openmdx%3Aorg.opencrx.kernel.activity1%2Fprovider%2F<%=providerName%>%2Fsegment%2F<%=segmentName%>%29*referenceName*%28addressGroup%29*filterByFeature*%28name%29*filterOperator*%28IS_LIKE%29*orderByFeature*%28name%29*position*%280%29*size*%2820%29',
                          {
                            paramName: 'filtervalues',
                            minChars: 2,
                            afterUpdateElement: updateXriField
                          }
                        );
                      </script>
                    </td>
                    <td class="addon">
                      <img class="popUpButton" border="1" alt="lookup" title="" src="../../images/<%=findAddressGroupObjectAction.getIconKey()%>" onclick="OF.findObject('../../<%=findAddressGroupObjectAction.getEncodedHRef(requestId)%>', document.forms['bulkEmail'].elements['bulkRecipientGroupXri.Title'], document.forms['bulkEmail'].elements['bulkRecipientGroupXri'], '<%=addressGroupLookupId%>');" />
                    </td>
                  </tr>
                  <tr>
                    <td class="<%= CssClass.fieldLabel %>" title="\${#} will be replace with activity number"><span class="nw"><%=userView.getFieldLabel(EMAIL_CLASS, "messageSubject", app.getCurrentLocaleAsIndex())%>&nbsp;&nbsp;&mdash;&nbsp;\${#}:</span></td>
                    <td>
                      <input type="text" id="messageSubject" name="messageSubject" class="valueL" style="font-family:courier;" tabindex="70" value="<%=messageSubject%>" />
                    </td>
                    <td class="addon" ></td>
                  </tr>
                  <tr>
                    <td class="<%= CssClass.fieldLabel %>"><span class="nw"><%=userView.getFieldLabel(EMAIL_CLASS, "messageBody", app.getCurrentLocaleAsIndex())%>:</span></td>
                    <td>
                      <textarea id="messageBody" name="messageBody" rows="6" style="font-family:courier;" tabindex="80"><%=messageBody%></textarea>
                    </td>
                    <td class="addon" ></td>
                  </tr>
                  <tr>
                    <td>&nbsp;</td>
                    <td></td>
                  </tr>

                  <tr>
                    <td class="<%= CssClass.fieldLabel %>"><span class="nw"><%=userView.getFieldLabel(EMAIL_CLASS, "gateway", app.getCurrentLocaleAsIndex())%>:</span></td>
                    <td>
                      <div class="autocompleterMenu">
                        <ul id="<%=CssClass.ssf_nav%>" class="<%=CssClass.ssf_nav%>" onmouseover="sfinit(this);" >
                          <li><a href="#"><img border="0" alt="" src="../../images/autocomplete_select.png" /></a>
                            <ul onclick="this.style.left='-999em';" onmouseout="this.style.left='';">
                              <li class="selected"><a href="#" onclick="javascript:navSelect(this);ac10.url= getEncodedHRef(['../../ObjectInspectorServlet', 'event', '40', 'parameter', 'xri*(xri:@openmdx:org.opencrx.kernel.account1/provider/<%= providerName %>/segment/<%= segmentName %>)*referenceName*(address)*filterByType*(org:opencrx:kernel:account1:AccountAddress)*filterByFeature*(emailAddress)*filterOperator*(IS_LIKE)*orderByFeature*(emailAddress)*position*(0)*size*(20)']);return false;"><span>&nbsp;&nbsp;&nbsp;</span><%= app.getLabel(EMAILADDRESS_CLASS) %></a></li>
                            </ul>
                          </li>
                        </ul>
                      </div>
                      <div class="autocompleterInput">
<%
                        String gatewayEmailLookupId = org.opencrx.kernel.backend.Activities.getInstance().getUidAsString();
                        findEmailAddressObjectAction = Action.getFindObjectAction("org:opencrx:kernel:account1:Segment:address", gatewayEmailLookupId);
%>
                        <input type="text" class="valueL valueAC" id="gatewayEmailXri.Title" name="gatewayEmailXri.Title" tabindex="90" value="<%= (gatewayEmail != null) && (gatewayEmail.getEmailAddress() != null) ? gatewayEmail.getEmailAddress() : "" %>" />
                      </div>
                      <input type="hidden" class="valueLLocked" id="gatewayEmailXri" name="gatewayEmailXri" readonly value="<%= gatewayEmailXri != null ? gatewayEmailXri : "" %>" />
                      <div class="autocomplete" id="gatewayEmailXri.Update" style="display:none;z-index:500;"></div>
                      <script type="text/javascript" language="javascript" charset="utf-8">
                        ac10 = new Ajax.Autocompleter(
                          'gatewayEmailXri.Title',
                          'gatewayEmailXri.Update',
                          '../../ObjectInspectorServlet?event=40&parameter=xri*%28xri%3A%40openmdx%3Aorg.opencrx.kernel.account1%2Fprovider%2F<%= providerName %>%2Fsegment%2F<%= segmentName %>%29*referenceName*%28address%29*filterByType*%28org:opencrx:kernel:account1:AccountAddress%29*filterByFeature*%28emailAddress%29*filterOperator*%28IS_LIKE%29*orderByFeature*%28emailAddress%29*position*%280%29*size*%2820%29',
                          {
                            paramName: 'filtervalues',
                            minChars: 2,
                            afterUpdateElement: updateXriField
                          }
                        );
                      </script>
                    </td>
                    <td class="addon">
                      <img class="popUpButton" border="1" alt="lookup" title="" src="../../images/<%= findEmailAddressObjectAction.getIconKey() %>" onclick="OF.findObject('../../<%= findEmailAddressObjectAction.getEncodedHRef(requestId) %>', document.forms['bulkEmail'].elements['gatewayEmailXri.Title'], document.forms['bulkEmail'].elements['gatewayEmailXri'], '<%= gatewayEmailLookupId %>');" />
                    </td>
                  </tr>

                </table>
              </fieldset>

              <fieldset>
            		<legend><span><img border="0" src="../../images/Document.gif" alt="Attachment(s)" /></span></legend>
                <input type="file" size="50" name="uploadFile" tabindex="600" /><INPUT type="Submit" name="Verify.Button" tabindex="601" value="Upload" />
                <hr>
<%
                int j = 0;
                for (Iterator i = filenames.iterator(); i.hasNext();) {
                  String filename = (String)i.next();
                  // mimeType and name
                  BufferedReader r = new BufferedReader(
                    new FileReader(filename + ".INFO")
                  );
                  String contentMimeType = r.readLine();
                  String contentName = r.readLine();
                  r.close();
%>
                  <div>
                    <input type="hidden" name="filename<%= j %>" value="<%= filename %>">
                    <b><%= contentName %></b> [<%= contentMimeType %>] <input type="button" value="X" onclick="javascript:this.parentNode.parentNode.removeChild( this.parentNode );" >
                  </div>
<%
                  j++;
                }
%>
              </fieldset>


              <table>
                <tr style="vertical-align:top;">
                  <td>
                    <input type="Submit" name="Verify.Button" class="<%= CssClass.btn.toString() %> <%= CssClass.btn_light.toString() %>" tabindex="10000" value="Verify" />
                  </td>
                  <td style="padding-left:5px;">
<%
                    if (!sendOk) {
%>
                      <div style="background-color:#F4FD48;border:1px solid #CCCCCC;">
                        <table>
                          <tr style="background-color:#eee;">
                            <td class="<%= CssClass.fieldLabel %>"><b>Field</b></td>
                            <td><b>Error</b></td>
                          </tr>
<%
                          if (currentActivityCreator == null) {
%>
                            <tr>
                              <td><%= app.getLabel(ACTIVITYCREATOR_CLASS) %></td>
                              <td>required selection - N/A not allowed</td>
                            </tr>
<%
                          }
                          if (!senderOk) {
%>
                            <tr>
                              <td><%= (codes.getLongTextByCode(featurePartyTypeEMailRecipient, app.getCurrentLocaleAsIndex(), true).get(CODE_FROM)) %></td>
                              <td>required field - bad value</td>
                            </tr>
<%
                          }
                          if (!recipientOk) {
%>
                            <tr>
                              <td><%= (codes.getLongTextByCode(featurePartyTypeEMailRecipient, app.getCurrentLocaleAsIndex(), true).get(CODE_TO)) %> (<%= app.getLabel(ADDRESSGROUP_CLASS) %>)</td>
                              <td>required field - bad value</td>
                            </tr>
<%
                          }
                          if (!bulkRecipientGroupOk) {
%>
                            <tr>
                              <td><%= (codes.getLongTextByCode(featurePartyTypeEMailRecipient, app.getCurrentLocaleAsIndex(), true).get(CODE_BCC)) %></td>
                              <td>required field - bad value</td>
                            </tr>
<%
                          }
%>
                        </table>
                      </div>
<%
                    }
%>
                  </td>
                </tr>
              </table>
              <br>
              <input type="checkbox" name="commitAndSend.CheckBox" value="commitAndSend" tabindex="10010" <%= commitAndSend ? "checked" : "" %> /> Send and close<br><br>
              <input type="Submit" name="Ok.Button" class="<%= CssClass.btn.toString() %> <%= CssClass.btn_light.toString() %>" tabindex="10020" value="<%= app.getTexts().getSaveTitle() %>" <%= sendOk ? "" : "disabled" %> />
              <input type="Submit" name="Cancel.Button" class="<%= CssClass.btn.toString() %> <%= CssClass.btn_light.toString() %>" tabindex="10030" value="<%= app.getTexts().getCancelTitle() %>" /><br>
            </form>
<%
          }
        }
        catch(Exception e) {
          try {
            pm.currentTransaction().rollback();
      		} catch (Exception e1) {}
          SysLog.warning("Error creating bulk e-mail", FORM_ACTION);
          SysLog.warning(e.getMessage(), e.getCause());
          // Go back to previous view
          Action nextAction = new ObjectReference(
            (RefObject_1_0)pm.getObjectById(new Path(objectXri)),
            app
          ).getSelectObjectAction();
          /*
          response.sendRedirect(
             request.getContextPath() + "/" + nextAction.getEncodedHRef()
          );
          */
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
