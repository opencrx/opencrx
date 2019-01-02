<%@  page contentType="text/html;charset=UTF-8" language="java" pageEncoding="UTF-8" %><%
/*
 * ====================================================================
 * Project:     openCRX/Core, http://www.openmdx.org/
 * Description: UploadEMail
 * Owner:       CRIXP Corp., Switzerland, http://www.crixp.com
 * ====================================================================
 *
 * This software is published under the BSD license
 * as listed below.
 *
 * Copyright (c) 2005-2014, CRIXP Corp., Switzerland
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
	<title>openCRX - Upload E-Mail</title>
	<meta http-equiv="Content-Type" content="text/html; charset=utf-8">
	<link rel="stylesheet" href="../../js/bootstrap/css/bootstrap.min.css">	
	<link rel="stylesheet" href="../../_style/colors.css">
	<link rel="stylesheet" href="../../_style/n2default.css">
	<link rel="stylesheet" href="../../_style/ssf.css">
	<script language="javascript" type="text/javascript" src="../../js/portal-all.js"></script>
	<link rel='shortcut icon' href='../../images/favicon.ico' />
	<style type="text/css" media="all">
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
				final String UPLOAD_FILE_FIELD_NAME = "uploadFile";
				List filecb = new ArrayList();
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
		
					// Get object
					String[] objectXris = (String[])parameterMap.get("xri");
					String objectXri = (objectXris == null) || (objectXris.length == 0) ? "" : objectXris[0];
					if(objectXri == null || app == null || viewsCache.getView(requestId) == null) {
						response.sendRedirect(
							request.getContextPath() + "/" + WebKeys.SERVLET_NAME
						);
						return;
					}
					RefObject_1_0 obj = (RefObject_1_0)pm.getObjectById(new Path(objectXri));
					String providerName = obj.refGetPath().get(2);
					String segmentName = obj.refGetPath().get(4);
					org.opencrx.kernel.account1.jmi1.Segment accountSegment = 
						org.opencrx.kernel.backend.Accounts.getInstance().getAccountSegment(pm, providerName, segmentName);

					boolean actionOk = parameterMap.get("OK.Button") != null;
					boolean actionCancel = parameterMap.get("Cancel.Button") != null;
		
					boolean replaceExisting = parameterMap.get("ReplaceExisting.CheckBox") != null;
					//System.out.println("replaceExisting=" + replaceExisting);

					
					// get file paths/names of files that had errors
					List<String> roundtripFilesPath = new ArrayList<String>();
					List<String> roundtripFilesName = new ArrayList<String>();
					int fileIdx = 0;
					while ((String[])parameterMap.get("filepath-" + fileIdx) != null) {
						try {
							String[] paths = (String[])parameterMap.get("filepath-" + fileIdx);
							roundtripFilesPath.add(paths[0]);
							String[] names = (String[])parameterMap.get("filename-" + fileIdx);
							roundtripFilesName.add(names[0]);
						} catch (Exception e) {}
						fileIdx++;
					}

					// get unmatched/provided e-mail addresses and build address map
					Map<String,String> addressMap = new TreeMap<String,String>(); // Mapping X.500 --> SMTP
					int emailIdx = 0;
					while ((String[])parameterMap.get("email-" + emailIdx) != null) {
						try {
							String[] provided = (String[])parameterMap.get("email-" + emailIdx);
							String[] unmatched = (String[])parameterMap.get("unmatched-" + emailIdx);
							if (!unmatched[0].isEmpty() && !provided[0].isEmpty()) {
								addressMap.put(unmatched[0], provided[0]);
							}
						} catch (Exception e) {}
						emailIdx++;
					}

					boolean hasErrors = false;
					List<String> errors = new ArrayList<String>();
					List<String> existingFilesPath = new ArrayList<String>();
					List<String> existingFilesName = new ArrayList<String>();

					String location = app.getTempFileName("1." + UPLOAD_FILE_FIELD_NAME, "");
		
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
						RefObject_1_0 navigateTo = (RefObject_1_0)pm.getObjectById(new Path(objectXri));
						int fileCounter = 1;
						int roundtripFilesCounter = 0;
						location = app.getTempFileName(fileCounter + "." + UPLOAD_FILE_FIELD_NAME, "");
						while(
							(
								new File(location + ".INFO").exists() &&
								new File(location).exists() &&
								(new File(location).length() > 0)
							)
							||
							(
								roundtripFilesPath.size() > roundtripFilesCounter &&
								roundtripFilesName.size() > roundtripFilesCounter
							)
						) {
							int preErrorCount = errors.size();

							String contentName = null;
							String contentMimeType = null;
							String providedEMailAddress = null;
							
							boolean processNewlyAddedTempFile = (
								new File(location + ".INFO").exists() &&
								new File(location).exists() &&
								(new File(location).length() > 0)
							);
							boolean processRoundtripFile = false;

							if (processNewlyAddedTempFile) {
								// mimeType and name
								BufferedReader r = new BufferedReader(
									new FileReader(location + ".INFO")
								);
								contentMimeType = r.readLine();
								contentName = r.readLine();
								r.close();
								new File(location + ".INFO").delete();
							} else {
								// fetch next roundtripFile
								location = roundtripFilesPath.get(roundtripFilesCounter);
								contentMimeType = "dummy";
								contentName = roundtripFilesName.get(roundtripFilesCounter);
								roundtripFilesCounter++;
								processRoundtripFile = true;
							}
							if(
								(contentName != null) &&
								(contentName.length() > 0) &&
								(contentMimeType != null) &&
								(contentMimeType.length() > 0)
							) {
								try {
									boolean isChecked = false;
									if (processNewlyAddedTempFile) {
										try {
											isChecked = filecb.get(fileCounter-1) != null && ((Boolean)filecb.get(fileCounter-1)).booleanValue();
										} catch (Exception e) {}
									} else {
										isChecked = true;
									}
									if (isChecked) {
										javax.mail.internet.MimeMessage msg = null;
										System.out.println("addressMap = " + addressMap);
										
										// MSG
										if(contentName != null && contentName.toUpperCase().endsWith(".MSG")) {
											List<String> newErrors = new ArrayList<String>();
											msg = org.opencrx.kernel.utils.MimeUtils.mapMsgToMime(
												new FileInputStream(location),
												accountSegment,
												addressMap, //Collections.<String,String>emptyMap(),
												false, // validateMappedAddresses (note that missing SMTP-Addresses are created with admin-Standard)
												newErrors
											);
											if (!newErrors.isEmpty()) {
												System.out.println("errors: " + newErrors);
												errors.addAll(newErrors);
											}
										}
										// MIME/EML
										else {
											msg = new org.opencrx.kernel.utils.MimeUtils.MimeMessageImpl(
												new FileInputStream(location)
											);																							
										}

										if(preErrorCount == errors.size() && msg != null) {
											List<org.opencrx.kernel.activity1.jmi1.EMail> emails = org.opencrx.kernel.backend.Activities.getInstance().importMimeMessage(
												pm,
												providerName,
												segmentName,
												msg,
												obj instanceof org.opencrx.kernel.activity1.jmi1.ActivityCreator ? (org.opencrx.kernel.activity1.jmi1.ActivityCreator)obj : null
											);
											new File(location).delete();
											navigateTo = (RefObject_1_0 )(emails != null && !emails.isEmpty() ? emails.iterator().next() : obj);
										} else {
											hasErrors = true;
											// preserve file path/name for roundtrip
											existingFilesPath.add(location);
											existingFilesName.add(contentName);
										}
									}
								}
								catch(Exception e) {
									ServiceException ex = new ServiceException(e);
									ex.log();
									out.println("<p><b>!! Failed !!<br><br>The following exception(s) occured:</b><br><br><pre>");
									PrintWriter pw = new PrintWriter(out);
									ex.printStackTrace(pw);
									out.println("</pre></p>");
									try {
										pm.currentTransaction().rollback();
									} catch(Exception e0) {}
								}
							}
							fileCounter++;
							location = app.getTempFileName(fileCounter + "." + UPLOAD_FILE_FIELD_NAME, "");
						}
						if (!hasErrors) {
							// no errors - leave wizard
							Action nextAction = new ObjectReference(
									navigateTo,
									app
								).getSelectObjectAction();
								response.sendRedirect(
									request.getContextPath() + "/" + nextAction.getEncodedHRef()
								);
						}
					}
					else {
						File uploadFile = new File(location);
						System.out.println("UploadEMail: file " + location + " either does not exist or has size 0: exists=" + uploadFile.exists() + "; length=" + uploadFile.length());
					}
					UserDefinedView userView = new UserDefinedView(
						pm.getObjectById(new Path(objectXri)),
						app,
						viewsCache.getView(requestId)
					);
					pm.close();
					
%>
					<form name="UploadEMail" enctype="multipart/form-data" accept-charset="UTF-8" method="POST" action="<%= "../.." + request.getServletPath() %>">
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
									<div id="etitle" style="height:40px;">
										<%= app.getLabel("org:opencrx:kernel:activity1:EMail") %>
									</div>
									<div class="col1"><fieldset>
										<table class="fieldGroup">
								 			<tr>
								 				<td class="<%= CssClass.fieldLabel %>"><span class="nw"><%= userView.getFieldLabel("org:opencrx:kernel:document1:Media", "content", app.getCurrentLocaleAsIndex()) %>:</span></td>
								 				<td >
								 					<input name="<%= UPLOAD_FILE_FIELD_NAME %>" id="<%= UPLOAD_FILE_FIELD_NAME %>" style="border:1px solid #ddd;" title="drop files here" type="file" multiple="multiple" tabindex="200" onChange="javascript:makeFileList();" />
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
												<td class="addon" ></td>
											</tr>
<%
											if (hasErrors) {
												for (int idx = 0; idx < errors.size(); idx++) {
													String providedValue = "";
													if (addressMap.get(errors.get(idx)) != null) {
														providedValue = (String)addressMap.get(errors.get(idx));
													}
													boolean isX500 = errors.get(idx) != null && errors.get(idx).startsWith("/");
%>
													<tr>
										 				<td class="<%= CssClass.fieldLabel %>"><span class="nw"><%= userView.getFieldLabel("org:opencrx:kernel:activity1:EMailRecipient", "party", app.getCurrentLocaleAsIndex()) %>:</span></td>
														<td nowrap>
<%
															if (isX500) {
%>
																<input type="text" name="email-<%= idx %>" size="20" value="<%= providedValue %>" style="width:250px;" />
<%
															} else {
%>
																<input type="text" name="email-<%= idx %>" size="20" value="<%= errors.get(idx) %>"  style="width:250px;" />
<%
															}
%>
															<input type="hidden" name="unmatched-<%= idx %>" value="<%= errors.get(idx) %>" /> 
											 				<span style="white-space:nowrap;overflow:hidden;" title="<%= errors.get(idx) %>"><%= errors.get(idx) %></span>
														</td>
														<td class="addon" ></td>
													</tr>
<%
												}													
											}
%>
											<tr>
												<td colspan="3">
													<br>
													<input type="Submit" name="OK.Button" class="<%= CssClass.btn.toString() %> <%= CssClass.btnDefault.toString() %>" tabindex="1000" value="<%= app.getTexts().getSaveTitle() %>" />
													<input type="Submit" name="Cancel.Button" class="<%= CssClass.btn.toString() %> <%= CssClass.btnDefault.toString() %>" tabindex="1010" value="<%= app.getTexts().getCancelTitle() %>" />
												</td>
											</tr>
										</table>
									</fieldset></div>
								</td>
							</tr>
						</table>
<%
						if (hasErrors) { 
							// preserve paths and names of existing files for roundtrip
							for (int idx = 0; idx < existingFilesPath.size(); idx++) {
%>
								<input type="hidden" name="filepath-<%= idx %>" value="<%= existingFilesPath.get(idx) %>" />
								<input type="hidden" name="filename-<%= idx %>" value="<%= existingFilesName.get(idx) %>" />
<%
							}
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
				}
%>
			</div> <!-- content -->
		</div> <!-- content-wrap -->
	</div> <!-- wrap -->
</div> <!-- container -->
</body>
</html>
