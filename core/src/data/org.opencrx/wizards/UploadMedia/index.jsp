<%@  page contentType="text/html;charset=UTF-8" language="java" pageEncoding="UTF-8" %><%
/*
 * ====================================================================
 * Project:     openCRX/Core, http://www.openmdx.org/
 * Description: UploadMedia
 * Owner:       CRIXP AG, Switzerland, http://www.crixp.com
 * ====================================================================
 *
 * This software is published under the BSD license
 * as listed below.
 *
 * Copyright (c) 2004-2014, CRIXP AG, Switzerland
 * All rights reserved.
 *
 * Redistribution and use in source and binary forms, with or
 * without modification, are permitted provided that the following
 * conditions are met:
 *
 * * Redistributions of source code must retain the above copyright
 * notice, this list of conditions and the following disclaimer.
 *
 * * Redistributions in binary form must reproduce the above copyright
 * notice, this list of conditions and the following disclaimer in
 * the documentation and/or other materials provided with the
 * distribution.
 *
 * * Neither the name of the openMDX team nor the names of its
 * contributors may be used to endorse or promote products derived
 * from this software without specific prior written permission.
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
 * This product includes software developed by Mihai Bazon
 * (http://dynarch.com/mishoo/calendar.epl) published with an LGPL
 * license.
 */
%><%@ page session="true" import="
java.util.*,
java.io.*,
java.text.*,
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
org.openmdx.kernel.log.*,
org.openmdx.kernel.exception.BasicException,
org.openmdx.uses.org.apache.commons.fileupload.*,
org.openmdx.kernel.id.*
" %><%
	request.setCharacterEncoding("UTF-8");
	ApplicationContext app = (ApplicationContext)session.getValue(WebKeys.APPLICATION_KEY);
	Texts_1_0 texts = app.getTexts();
%>
<!DOCTYPE HTML PUBLIC "-//W3C//DTD HTML 4.01 Transitional//EN" "http://www.w3.org/TR/html4/loose.dtd">
<html dir="<%= texts.getDir() %>">
<head>
	<title>openCRX - Upload Media</title>
	<meta http-equiv="Content-Type" content="text/html; charset=utf-8">
	<link rel="stylesheet" href="../../js/bootstrap/css/bootstrap.min.css">	
	<link rel="stylesheet" href="../../_style/colors.css">
	<link rel="stylesheet" href="../../_style/n2default.css">
	<link rel="stylesheet" href="../../_style/ssf.css">
	<script language="javascript" type="text/javascript" src="../../js/portal-all.js"></script>
	<link rel='shortcut icon' href='../../images/favicon.ico' />
 	<style type="text/css" media="all">
    /* Add/Edit page specific settings */
    .col1 {float: left; width: 99%;}
  </style>
</head>
<body>
<div id="container">
	<div id="wrap">
		<div id="eheader">
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
    	<div id="econtent">

<%
		final String MEDIA_CLASS = "org:opencrx:kernel:document1:Media";
		final String MEDIACONTENT_CLASS = "org:opencrx:kernel:document1:MediaContent";
		final String UPLOAD_FILE_FIELD_NAME = "uploadFile";
		List filecb = new ArrayList();
		String errorMsg = "";
		String errorTitle = "";
		try {

			Map parameterMap = request.getParameterMap();
	    	if(FileUpload.isMultipartContent(request)) {
				parameterMap = new HashMap();
				DiskFileUpload upload = new DiskFileUpload();
				upload.setHeaderEncoding("UTF-8");
				try {
					List items = upload.parseRequest(
						request,
						200,  // in-memory threshold. Content for fields larger than threshold is written to disk
						50000000, // max request size [overall limit]
					  app.getTempDirectory().getPath()
					);
					int fileCounter = 0;
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
								fileCounter++;
						  parameterMap.put(
							item.getFieldName(),
							new String[]{item.getName()}
						  );
							  String location = app.getTempFileName(fileCounter + "." + item.getFieldName(), "");

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
						//System.out.println("location = " + location + " / name = " + item.getName().substring(sep + 1));
						  pw.close();
						}
					  }
					}
					int recount = 1;
					while (recount <= fileCounter) {
						boolean isChecked = parameterMap.get("filecb" + recount) != null;
						filecb.add(new Boolean(isChecked));
						recount++;
					}
				}
				catch(FileUploadException e) {
					SysLog.warning("can not upload file", e.getMessage());
				}
			}

			ViewsCache viewsCache = (ViewsCache)session.getValue(WebKeys.VIEW_CACHE_KEY_SHOW);
			String[] requestIds = (String[])parameterMap.get(Action.PARAMETER_REQUEST_ID);
			String requestId = (requestIds == null) || (requestIds.length == 0) ? "" : requestIds[0];
			javax.jdo.PersistenceManager pm = app.getNewPmData();

			boolean actionOk = parameterMap.get("OK.Button") != null;
			boolean actionCancel = parameterMap.get("Cancel.Button") != null;

			String[] descriptions = (String[])parameterMap.get("description");
			String description = (descriptions == null) || (descriptions.length == 0) ? "" : descriptions[0];

			boolean replaceExisting = parameterMap.get("ReplaceExisting.CheckBox") != null;
			//System.out.println("replaceExisting=" + replaceExisting);

			String[] objectXris = (String[])parameterMap.get("xri");
			String objectXri = (objectXris == null) || (objectXris.length == 0) ? "" : objectXris[0];
			String location = app.getTempFileName("1." + UPLOAD_FILE_FIELD_NAME, "");

			if(objectXri == null || app == null || viewsCache.getView(requestId) == null) {
				response.sendRedirect(
					request.getContextPath() + "/" + WebKeys.SERVLET_NAME
				);
				return;
			}
			UserDefinedView userView = new UserDefinedView(
					pm.getObjectById(new Path(objectXri)),
					app,
					viewsCache.getView(requestId)
				);

			if(actionCancel) {
				Action nextAction = new ObjectReference(
					(RefObject_1_0)pm.getObjectById(new Path(objectXri)),
					app
				).getSelectObjectAction();
				response.sendRedirect(
					request.getContextPath() + "/" + nextAction.getEncodedHRef()
				);
			}
			else if(actionOk) {

				int fileCounter = 1;
				location = app.getTempFileName(fileCounter + "." + UPLOAD_FILE_FIELD_NAME, "");
				while(
					new File(location + ".INFO").exists() &&
					new File(location).exists() &&
					(new File(location).length() > 0)
				) {
					 // mimeType and name
					 BufferedReader r = new BufferedReader(
					   new FileReader(location + ".INFO")
					 );
					 String contentMimeType = r.readLine();
					 String contentName = r.readLine();
					 r.close();
					new File(location + ".INFO").delete();

					if(
						(contentName != null) &&
						(contentName.length() > 0) &&
						(contentMimeType != null) &&
						(contentMimeType.length() > 0)
					) {
						try {
							pm.currentTransaction().begin();

							RefObject_1_0 obj = (RefObject_1_0)pm.getObjectById(new Path(objectXri));

							boolean isChecked = false;
							try {
									isChecked = filecb.get(fileCounter-1) != null && ((Boolean)filecb.get(fileCounter-1)).booleanValue();
							} catch (Exception e) {}

							// CrxObject
							if (isChecked && obj instanceof org.opencrx.kernel.generic.jmi1.CrxObject) {
								org.opencrx.kernel.generic.jmi1.CrxObject crxObject =
									(org.opencrx.kernel.generic.jmi1.CrxObject)obj;
								org.opencrx.kernel.generic.jmi1.Media media = null;
								if(replaceExisting) {
									for(Iterator i = crxObject.getMedia().iterator(); i.hasNext(); ) {
										org.opencrx.kernel.generic.jmi1.Media m = (org.opencrx.kernel.generic.jmi1.Media)i.next();
										if(m.getContentName().equals(contentName)) {
											media = m;
											break;
										}
									}
								}
								boolean isNew = false;
								if(media == null) {
									media = pm.newInstance(org.opencrx.kernel.generic.jmi1.Media.class);
									isNew = true;
								}
								if(isNew) {
									media.setDescription(description.length() > 0 ? description : contentName);
								}
								media.setContentName(contentName);
								media.setContentMimeType(contentMimeType);
								media.setContent(
									org.w3c.cci2.BinaryLargeObjects.valueOf(new File(location))
								);
								if(isNew) {
									crxObject.addMedia(
										false,
										org.opencrx.kernel.backend.Activities.getInstance().getUidAsString(),
										media
									);
								}
								if (obj instanceof org.opencrx.kernel.activity1.jmi1.Activity) {
									// touch Activity to trigger change in modifiedAt
									org.opencrx.kernel.utils.Utils.touchObject(obj);
								}
							}
							// UserHome
							else if (isChecked && obj instanceof org.opencrx.kernel.home1.jmi1.UserHome) {
							    org.opencrx.kernel.home1.jmi1.UserHome userHome =
									(org.opencrx.kernel.home1.jmi1.UserHome)obj;
								org.opencrx.kernel.home1.jmi1.Media media = null;
								if(replaceExisting) {
									for(Iterator i = userHome.getChart().iterator(); i.hasNext(); ) {
										org.opencrx.kernel.home1.jmi1.Media m = (org.opencrx.kernel.home1.jmi1.Media)i.next();
										if(m.getContentName().equals(contentName)) {
											media = m;
											break;
										}
									}
								}
								boolean isNew = false;
								if(media == null) {
									media = pm.newInstance(org.opencrx.kernel.home1.jmi1.Media.class);
									isNew = true;
								}
								if(isNew) {
									media.setDescription(description.length() > 0 ? description : contentName);
								}
								media.setContentName(contentName);
								media.setContentMimeType(contentMimeType);
								media.setContent(
									org.w3c.cci2.BinaryLargeObjects.valueOf(new File(location))
								);
								if(isNew) {
								    userHome.addChart(
										false,
										org.opencrx.kernel.backend.Activities.getInstance().getUidAsString(),
										media
									);
								}
							}
							else if (isChecked && obj instanceof org.opencrx.kernel.document1.jmi1.Document) {
								org.opencrx.kernel.document1.jmi1.Document document =
									(org.opencrx.kernel.document1.jmi1.Document)obj;
								org.opencrx.kernel.document1.jmi1.DocumentAttachment documentAttachment = null;
								if(replaceExisting) {
									for(Iterator i = document.getAttachment().iterator(); i.hasNext(); ) {
										org.opencrx.kernel.document1.jmi1.DocumentAttachment a = (org.opencrx.kernel.document1.jmi1.DocumentAttachment)i.next();
										if(a.getContentName().equals(contentName)) {
											documentAttachment = a;
											break;
										}
									}
								}
								// Add media to document object
								boolean isNew = false;
								if(documentAttachment == null) {
									documentAttachment = pm.newInstance(org.opencrx.kernel.document1.jmi1.DocumentAttachment.class);
									isNew = true;
								}
								documentAttachment.setName(description.length() > 0 ? description : contentName);
								if(isNew) {
									documentAttachment.setDescription(description.length() > 0 ? description : contentName);
								}
								documentAttachment.setContentName(contentName);
								documentAttachment.setContentMimeType(contentMimeType);
								documentAttachment.setContent(
									org.w3c.cci2.BinaryLargeObjects.valueOf(new File(location))
								);
								if(isNew) {
									document.addAttachment(
										false,
										org.opencrx.kernel.backend.Activities.getInstance().getUidAsString(),
										documentAttachment
									);
								}
							}

							pm.currentTransaction().commit();
							new File(location).delete();
						}
						catch(Exception e) {
							try {
								errorMsg = "ERROR - cannot upload " + app.getLabel(MEDIACONTENT_CLASS);
								Throwable err = e;
								while (err.getCause() != null) {
									err = err.getCause();
								}
								errorTitle += "<pre>" + err.toString() + "</pre>";
								errorMsg += "<br>" + errorTitle;
								pm.currentTransaction().rollback();
							} catch(Exception e0) {}
						}
					}
					fileCounter++;
					location = app.getTempFileName(fileCounter + "." + UPLOAD_FILE_FIELD_NAME, "");
				}
				if (errorMsg.length() == 0) {
					// return to calling object
					Action nextAction = new ObjectReference(
						(RefObject_1_0)pm.getObjectById(new Path(objectXri)),
						app
					).getSelectObjectAction();
					response.sendRedirect(
						request.getContextPath() + "/" + nextAction.getEncodedHRef()
					);
				}
			}
			else {
				File uploadFile = new File(location);
				System.out.println("UploadMedia: file " + location + " either does not exist or has size 0: exists=" + uploadFile.exists() + "; length=" + uploadFile.length());
			}
			pm.close();
%>
<form name="UploadMedia" enctype="multipart/form-data" accept-charset="UTF-8" method="POST" action="<%= "../.." + request.getServletPath() %>">
	<input type="hidden" name="<%= Action.PARAMETER_OBJECTXRI %>" value="<%= objectXri %>" />
	<input type="hidden" name="<%= Action.PARAMETER_REQUEST_ID %>" value="<%= requestId %>" />
<%
				if (errorMsg.length() > 0) {
%>
					<div title="<%= errorTitle.replace("\"", "'") %>"  style="background-color:red;color:white;border:1px solid black;padding:10px;font-weight:bold;margin-top:10px;">
						<%= errorMsg %>
					</div>
<%
				}
%>
<table cellspacing="8" class="tableLayout">
  <tr>
    <td class="cellObject">
      <noscript>
        <div class="panelJSWarning" style="display: block;">
          <a href="../../helpJsCookie.html" target="_blank"><img class="popUpButton" src="../../images/help.gif" width="16" height="16" border="0" onclick="javascript:void(window.open('helpJsCookie.html', 'Help', 'fullscreen=no,toolbar=no,status=no,menubar=no,scrollbars=yes,resizable=yes,directories=no,location=no,width=400'));" alt="" /></a> <%= texts.getPageRequiresScriptText() %>
        </div>
      </noscript>
      <div id="etitle" style="height:20px;">
        <%= app.getLabel(MEDIACONTENT_CLASS) %>
      </div>

      <div class="col1"><fieldset>
	      <table class="fieldGroup">
	        <tr>
	          <td class="<%= CssClass.fieldLabel %>"><span class="nw"><%= userView.getFieldLabel(MEDIA_CLASS, "description", app.getCurrentLocaleAsIndex()) %>:</span></td>
	          <td>
	            <input type="text" class="valueL" name="description" maxlength="50" tabindex="100" value="<%= description %>" />
	          </td>
	          <td class="addon"></td>
	        </tr>
     			<tr>
     				<td class="<%= CssClass.fieldLabel %>"><span class="nw"><%= userView.getFieldLabel(MEDIA_CLASS, "content", app.getCurrentLocaleAsIndex()) %>:</span></td>
     				<td >
     					<input name="<%= UPLOAD_FILE_FIELD_NAME %>" id="<%= UPLOAD_FILE_FIELD_NAME %>" style="border:1px solid #ddd;" title="drop files here" type="file" multiple="multiple" tabindex="200" onChange="javascript:makeFileList();" />
     					&nbsp;&nbsp;&nbsp;<input type="checkbox" name="ReplaceExisting.CheckBox" value="false" tabindex="300" />
<%
              switch (app.getCurrentLocaleAsIndex()) {
                case 0:  %><%= "Replace existing file(s) with same name" %><% break;
                case 1:  %><%= "Datei(en) mit gleichem Namen ersetzen"   %><% break;
                default: %><%= "Replace existing file(s) with same name" %><% break;
              }
%>
							<div id="fileList"></div>
							<script type="text/javascript">
								$('<%= UPLOAD_FILE_FIELD_NAME %>').style.height='75px';
							
								function makeFileList() {
									$('<%= UPLOAD_FILE_FIELD_NAME %>').style.height='';
									var input = $("<%= UPLOAD_FILE_FIELD_NAME %>");
									var outerdiv = $("fileList");
									while (outerdiv.hasChildNodes()) {
										outerdiv.removeChild(outerdiv.firstChild);
									}
									for (var i = 0; i < input.files.length; i++) {
										var div = document.createElement("div");
										var cb = document.createElement("input");
										cb.type = "checkbox";
										cb.name = "filecb"+(i+1);
						        cb.id = "filecb"+(i+1);
						        cb.value = input.files[i].name;
						        cb.checked = true;
						        var text = document.createTextNode(input.files[i].name);
										div.appendChild(cb);
										div.appendChild(text);
										outerdiv.appendChild(div);
									}
									if(!outerdiv.hasChildNodes()) {
										outerdiv.innerHTML = '--';
										$('<%= UPLOAD_FILE_FIELD_NAME %>').style.height='75px';
									}
								}
							</script>

     				</td>
     				<td class="addon" >
	        </tr>
     			<tr>
	          <td colspan="3">
	          	<br>
	          		<input type="Submit" name="OK.Button" class="<%= CssClass.btn.toString() %> <%= CssClass.btn_light.toString() %>" tabindex="1000" value="<%= app.getTexts().getSaveTitle() %>" />
      			  	<input type="Submit" name="Cancel.Button" class="<%= CssClass.btn.toString() %> <%= CssClass.btn_light.toString() %>" tabindex="1010" value="<%= app.getTexts().getCancelTitle() %>" />
	          </td>
	        </tr>
	      </table>
      </fieldset></div>
  	</td>
  </tr>
</table>
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
    }
%>
      </div> <!-- content -->
    </div> <!-- content-wrap -->
  </div> <!-- wrap -->
</div> <!-- container -->
</body>
</html>
