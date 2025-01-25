<%@  page contentType="text/html;charset=UTF-8" language="java" pageEncoding="UTF-8" %><%
/*
 * ====================================================================
 * Project:     openCRX/Core, http://www.opencrx.org/
 * Description: UploadDocument
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
java.util.*,
java.io.*,
java.text.*,
org.opencrx.kernel.generic.*,
org.opencrx.kernel.backend.*,
org.openmdx.kernel.id.cci.*,
org.openmdx.base.exception.*,
org.openmdx.base.accessor.jmi.cci.*,
org.openmdx.portal.servlet.*,
org.openmdx.portal.servlet.action.*,
org.openmdx.portal.servlet.attribute.*,
org.openmdx.portal.servlet.component.*,
org.openmdx.portal.servlet.control.*,
org.openmdx.portal.servlet.wizards.*,
org.openmdx.base.naming.*,
org.openmdx.kernel.log.*,
org.openmdx.uses.org.apache.commons.fileupload.*,
org.openmdx.kernel.id.*
" %><%
	request.setCharacterEncoding("UTF-8");
	ApplicationContext app = (ApplicationContext)session.getAttribute(WebKeys.APPLICATION_KEY);
	if (app == null) {
		System.out.println("aborting... (ApplicationContext == null)");
    	response.sendRedirect(
			request.getContextPath() + "/" + WebKeys.SERVLET_NAME
		);
		return;
	}
	boolean uploadFailed = false;
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
			SysLog.warning("first try to upload file failed", e.getMessage());
			new ServiceException(e).log();
		}
		try {
			if(uploadFailed) {
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
						String location = app.getTempFileName(item.getFieldName(), "");
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
			new ServiceException(e).log();
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
	ViewsCache viewsCache = (ViewsCache)session.getAttribute(WebKeys.VIEW_CACHE_KEY_SHOW);
	String[] objectXris = (String[])parameterMap.get("xri");
	String objectXri = (objectXris == null) || (objectXris.length == 0) ? null : objectXris[0];
	if (objectXri == null) {
		System.out.println("xri=null - reading again");
		objectXri = request.getParameter("xri");
	}
	if(objectXri == null || app == null || viewsCache.getView(requestId) == null) {
		response.sendRedirect(
			request.getContextPath() + "/" + WebKeys.SERVLET_NAME
		);
		return;
	}
	javax.jdo.PersistenceManager pm = app.getNewPmData();
	Texts_1_0 texts = app.getTexts();
	RefObject_1_0 object = (RefObject_1_0)pm.getObjectById(new Path(objectXri));
%>
<!DOCTYPE HTML PUBLIC "-//W3C//DTD HTML 4.01 Transitional//EN" "http://www.w3.org/TR/html4/loose.dtd">
<html dir="<%= texts.getDir() %>">
<head>
	<title><%= app.getApplicationName() + " - " + (new ObjectReference(object, app)).getTitle() + ((new ObjectReference(object, app)).getTitle().length() == 0 ? "" : " - ") + (new ObjectReference(object, app)).getLabel() %></title>
	<meta http-equiv="Content-Type" content="text/html; charset=UTF-8">
	<link rel="stylesheet" href="../../js/bootstrap/css/bootstrap.min.css">	
	<link rel="stylesheet" href="../../_style/colors.css">
	<link rel="stylesheet" href="../../_style/n2default.css">
	<link rel="stylesheet" href="../../_style/ssf.css">
	<link rel='shortcut icon' href='../../images/favicon.ico' />	
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
	var selectedObjTab = null;
	var panelsObj = new Array(
		'panelObj0'
	);
	</script>
	<link rel='shortcut icon' href='../../images/favicon.ico' />
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
				final String DOCUMENT_CLASS = "org:opencrx:kernel:document1:Document";
				final String DOCUMENTFOLDER_CLASS = "org:opencrx:kernel:document1:DocumentFolder";
				final String MEDIA_CLASS = "org:opencrx:kernel:document1:Media";
				final String PRINCIPAL_GROUP_CLASS = "org:opencrx:security:realm1:PrincipalGroup";
				try {
					org.openmdx.portal.servlet.Codes codes = app.getCodes();
					String featureDocumentTypeCode = "documentType";
					Map<String,Short> documentTypeCode_longTextsT = codes.getLongTextByText(featureDocumentTypeCode, app.getCurrentLocaleAsIndex(), true);
					String featureLiteratureTypeCode = "literatureType";
					Map<String,Short> literatureTypeCode_longTextsT = codes.getLongTextByText(featureLiteratureTypeCode, app.getCurrentLocaleAsIndex(), true);
					boolean actionOk = parameterMap.get("OK.Button") != null;
					boolean actionCancel = parameterMap.get("Cancel.Button") != null;
					boolean successfullyCreated = false;
					boolean isNewRevision = 
						(parameterMap.get("newRevision") != null) ||
						(object instanceof org.opencrx.kernel.document1.jmi1.Document);

					Path objectPath = new Path(objectXri);
					String providerName = objectPath.get(2);
					String segmentName = objectPath.get(4);
					org.opencrx.kernel.home1.jmi1.UserHome myUserHome = org.opencrx.kernel.backend.UserHomes.getInstance().getUserHome(objectPath, pm);

					String[] names = (String[])parameterMap.get("name");
					String name = (names == null) || (names.length == 0) ? "" : names[0];
					String[] descriptions = (String[])parameterMap.get("description");
					String description = (descriptions == null) || (descriptions.length == 0) ? "" : descriptions[0];
					String[] titles = (String[])parameterMap.get("title");
					String title = (titles == null) || (titles.length == 0) ? "" : titles[0];
					String[] authors = (String[])parameterMap.get("author");
					String author = (authors == null) || (authors .length == 0) ? myUserHome.refGetPath().getLastSegment().toString() : authors[0];
					String[] qualifiedNamess = (String[])parameterMap.get("qualifiedName");
					String qualifiedName = (qualifiedNamess == null) || (qualifiedNamess.length == 0) ? "" : qualifiedNamess[0];
					String[] searchTexts = (String[])parameterMap.get("searchText");
					String searchText = (searchTexts == null) || (searchTexts.length == 0) ? "" : searchTexts[0];
					String[] documentNumbers = (String[])parameterMap.get("documentNumber");
					String documentNumber = (documentNumbers == null) || (documentNumbers.length == 0) ? "" : documentNumbers[0];
					String[] documentFolders = (String[])parameterMap.get("documentFolder");
					String documentFolder = (documentFolders == null) || (documentFolders.length == 0) ? "" : documentFolders[0];
					String[] documentFolderTitles = (String[])parameterMap.get("documentFolder.Title");
					String documentFolderTitle = (documentFolderTitles == null) || (documentFolderTitles.length == 0) ? "" : documentFolderTitles[0];
					String[] documentTypes = (String[])parameterMap.get("documentType");
					String documentType = (documentTypes == null) || (documentTypes.length == 0) ? "0" : documentTypes[0];
					String[] literatureTypes = (String[])parameterMap.get("literatureType");
					String literatureType = (literatureTypes == null) || (literatureTypes.length == 0) ? "0" : literatureTypes[0];

					org.openmdx.security.realm1.jmi1.Realm realm = SecureObject.getInstance().getRealm(pm, providerName, segmentName);
					org.opencrx.kernel.home1.jmi1.Segment userHomeSegment = UserHomes.getInstance().getUserHomeSegment(pm, providerName, segmentName);
					org.opencrx.kernel.document1.jmi1.Segment documentSegment = Documents.getInstance().getDocumentSegment(pm, providerName, segmentName);
      				org.opencrx.security.realm1.jmi1.PrincipalGroup myPrincipalGroup = myUserHome.getPrimaryGroup();
      				org.opencrx.security.realm1.jmi1.PrincipalGroup admins = (org.opencrx.security.realm1.jmi1.PrincipalGroup)realm.getPrincipal(SecurityKeys.PRINCIPAL_GROUP_ADMINISTRATORS);
      				String[] owningGroup00s = (String[])parameterMap.get("owningGroup00");
      				String owningGroup00 = (owningGroup00s == null) || (owningGroup00s.length == 0) ? admins.refMofId() : owningGroup00s[0];
      				String[] owningGroup01s = (String[])parameterMap.get("owningGroup01");
      				String owningGroup01 = (owningGroup01s == null) || (owningGroup01s.length == 0) ? (myPrincipalGroup != admins ? myPrincipalGroup.refMofId() : "") : owningGroup01s[0];
      				String[] owningGroup02s = (String[])parameterMap.get("owningGroup02");
      				String owningGroup02 = (owningGroup02s == null) || (owningGroup02s.length == 0) ? "" : owningGroup02s[0];
      				
      				org.opencrx.kernel.document1.jmi1.Document document = null;
      				if(actionCancel || (objectXri == null)) {
      					Action nextAction = (new ObjectReference(object, app)).getSelectObjectAction();
      					response.sendRedirect(
      						request.getContextPath() + "/" + nextAction.getEncodedHRef()
      					);
      				} else if(actionOk && !uploadFailed) { 
      					try {
      						pm.currentTransaction().begin();
      						if (isNewRevision) {
      							document = (org.opencrx.kernel.document1.jmi1.Document)object;
      						} else {
      							document = pm.newInstance(org.opencrx.kernel.document1.jmi1.Document.class);
      							document.setName(name);
      							document.setDescription(description);
      							document.setTitle(title);
      							document.setAuthor(author);
      							document.setQualifiedName(qualifiedName);
      							document.setSearchText(searchText);
      							document.setDocumentNumber(documentNumber);
      							document.setDocumentType(Short.parseShort(documentType));
      							document.setLiteratureType(Short.parseShort(literatureType));
      							document.setActiveOn(new Date());
      							documentSegment.addDocument(
      								Base.getInstance().getUidAsString(),
      								document
      							);
      						}
      						// Add revision
      						String resourceLocation = app.getTempFileName("uploadFile", "");
      						BufferedReader r = new BufferedReader(
      							new FileReader(resourceLocation + ".INFO")
      						);
      						String contentMimeType = r.readLine();
      						String contentName = r.readLine();
      						r.close();      						
      						Documents.getInstance().addRevision(
      							document, 
      							contentName, 
      							contentMimeType, 
      							author, 
      							org.w3c.cci2.BinaryLargeObjects.valueOf(new File(resourceLocation))
      						);
							pm.currentTransaction().commit();
      						// Add to document folder and adjust security of document
      						if(!isNewRevision) {
      							org.opencrx.kernel.document1.jmi1.DocumentFolder folder = null;
      							if(object instanceof org.opencrx.kernel.document1.jmi1.DocumentFolder) {
      								folder = (org.opencrx.kernel.document1.jmi1.DocumentFolder)object;
      							} else {
	      							try {
	      								folder = (org.opencrx.kernel.document1.jmi1.DocumentFolder)pm.getObjectById(new Path(documentFolder));
	      							} catch(Exception ignore) {}
      							}
      							if(folder != null) {
          							try {
		      							pm.currentTransaction().begin();
	      								document.getFolder().add(folder);
		      							pm.currentTransaction().commit();
	      							} catch(Exception e) {
	      								try {
	      									pm.currentTransaction().rollback();
	      								} catch(Exception e1) {}
	      							}
      							}
      							// Adjust security
      							List<String> owningGroupXris = new ArrayList<String>();
      							if(object instanceof org.opencrx.kernel.document1.jmi1.DocumentFolder) {
      								for(org.opencrx.security.realm1.jmi1.PrincipalGroup owningGroup: ((org.opencrx.kernel.document1.jmi1.DocumentFolder)object).<org.opencrx.security.realm1.jmi1.PrincipalGroup>getOwningGroup()) {
      									owningGroupXris.add(owningGroup.refMofId());
      								}
      							} else {
	      							try {
	      								List<org.opencrx.security.realm1.jmi1.PrincipalGroup> owningGroups = document.getOwningGroup();
	      								for(org.opencrx.security.realm1.jmi1.PrincipalGroup owningGroup: owningGroups) {
	      									owningGroupXris.add(owningGroup.refMofId());
					          			}
	      							} catch (Exception ignore) {}
      							}
      							try {
      								pm.currentTransaction().begin();
      								for(String owningGroupXri: owningGroupXris) {
										org.opencrx.security.realm1.jmi1.PrincipalGroup owningGroup = (org.opencrx.security.realm1.jmi1.PrincipalGroup)pm.getObjectById(new Path(owningGroupXri));
										org.opencrx.kernel.base.jmi1.ModifyOwningGroupParams modifyOwningGroupParams = org.w3c.spi2.Structures.create(
											org.opencrx.kernel.base.jmi1.ModifyOwningGroupParams.class, 
											org.w3c.spi2.Datatypes.member(org.opencrx.kernel.base.jmi1.ModifyOwningGroupParams.Member.group, owningGroup),
											org.w3c.spi2.Datatypes.member(org.opencrx.kernel.base.jmi1.ModifyOwningGroupParams.Member.mode, (short)1)
										); 
										document.removeOwningGroup(modifyOwningGroupParams);
									}
      								pm.currentTransaction().commit();
      							} catch (Exception e) {
      								new ServiceException(e).log();
      								try {
      									pm.currentTransaction().rollback();
      								} catch(Exception e1) {}
      							}      							
      							// Add owningGroups
      							for(String owningGroupXri: Arrays.asList(owningGroup00, owningGroup01, owningGroup02)) {
	      							if ((owningGroupXri != null) && !owningGroupXri.isEmpty()) {
	      								try {
	      									pm.currentTransaction().begin();	      									
	      									org.opencrx.security.realm1.jmi1.PrincipalGroup group = (org.opencrx.security.realm1.jmi1.PrincipalGroup)pm.getObjectById(new Path(owningGroupXri));
	      									org.opencrx.kernel.base.jmi1.ModifyOwningGroupParams modifyOwningGroupParams = org.w3c.spi2.Structures.create(
    											org.opencrx.kernel.base.jmi1.ModifyOwningGroupParams.class, 
    											org.w3c.spi2.Datatypes.member(org.opencrx.kernel.base.jmi1.ModifyOwningGroupParams.Member.group, group),
    											org.w3c.spi2.Datatypes.member(org.opencrx.kernel.base.jmi1.ModifyOwningGroupParams.Member.mode, (short)1)
    										); 
	      									document.addOwningGroup(modifyOwningGroupParams);
	      									pm.currentTransaction().commit();
	      								} catch (Exception e) {
	      									new ServiceException(e).log();
	      									try {
	      										pm.currentTransaction().rollback();
	      									} catch(Exception e1) {}
	      								}
	      							}
      							}
			    			}
		    				successfullyCreated = true;
      					} catch(Exception e) {
      						try {
      							pm.currentTransaction().rollback();
      						} catch(Exception e0) {}
      						SysLog.warning("cannot upload file", e.getMessage());
%>
							<div style="margin:0;padding:10px 10px 10px 10px;background-color:#FF0000;color:#FFFFFF;">
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
      				if(!successfullyCreated || uploadFailed) {
      					boolean invalidTitle = actionOk && ((title == null) || (title.length() == 0));
      					boolean invalidDocumentNumber  = actionOk && ((documentNumber == null) || (documentNumber.length() == 0));
      					String styleModifier = isNewRevision ? 
      						"style='display:none;'"
      							: "";
      					UserDefinedView userView = new UserDefinedView(
      						object,
      						app,
      						viewsCache.getView(requestId)
      					);
      					int tabIndex = 100;
%>
<form name="UploadDocument" enctype="multipart/form-data" accept-charset="UTF-8" method="POST" action="<%= "../.." + request.getServletPath() %>">
<input type="hidden" name="xri" value="<%= objectXri %>" />
<input type="hidden" name="<%= Action.PARAMETER_REQUEST_ID %>" value="<%= requestId %>" />
<input type="checkbox" name="newRevision" <%= isNewRevision ? "checked" : "" %> value="newRevision" style="display:none;" />
<table cellspacing="8" class="tableLayout">
	<tr>
		<td class="cellObject">
			<div class="OperationDialogTitle">
				<%= isNewRevision ? userView.getFieldLabel(DOCUMENT_CLASS, "headRevision", app.getCurrentLocaleAsIndex()) : app.getLabel(DOCUMENT_CLASS) %>
			</div>
			<br />
			<div class="<%= CssClass.fieldGroupName %>">&nbsp;</div>			
			<table class="fieldGroup">
				<tr>
					<td class="<%= CssClass.fieldLabel %>"><span class="nw"><%= userView.getFieldLabel(MEDIA_CLASS, "content", app.getCurrentLocaleAsIndex()) %>:</span></td>
					<td>
						<input type="file" class="valueL" size=50 name="uploadFile" tabindex="<%= tabIndex++ %>" />
					</td>
					<td class="addon"></td>
					<td class="<%= CssClass.fieldLabel %>"></td>
					<td></td>
					<td class="addon"></td>					        
				</tr>
			</table>
<%
			if(!(object instanceof org.opencrx.kernel.document1.jmi1.Document)) {
%>				
				<div class="<%= CssClass.fieldGroupName %>">&nbsp;</div>			
				<table class="fieldGroup">
					<tr>
						<td class="<%= CssClass.fieldLabel %>"><span class="nw"><%= userView.getFieldLabel(DOCUMENT_CLASS, "name", app.getCurrentLocaleAsIndex()) %>:</span></td>
						<td>
							<input type="text" class="valueL" name="name" maxlength="50" tabindex="<%= tabIndex++ %>" value="<%= name %>" />
						</td>
						<td class="addon"></td>
						<td class="<%= CssClass.fieldLabel %>"></td>
						<td></td>
						<td class="addon"></td>
					</tr>
					<tr>
						<td class="<%= CssClass.fieldLabel %>"><span class="nw"><%= userView.getFieldLabel(DOCUMENT_CLASS, "description", app.getCurrentLocaleAsIndex()) %>:</span></td>
						<td>
							<input type="text" class="valueL" name="description" maxlength="50" tabindex="<%= tabIndex++ %>" value="<%= description %>" />
						</td>
						<td class="addon"></td>
						<td class="<%= CssClass.fieldLabel %>"></td>
						<td></td>
						<td class="addon"></td>
					</tr>
					<tr>
						<td class="<%= CssClass.fieldLabel %>"><span class="nw"><%= userView.getFieldLabel(DOCUMENT_CLASS, "title", app.getCurrentLocaleAsIndex()) %>:</span></td>
						<td>
							<input type="text" class="valueL" name="title" maxlength="50" tabindex="<%= tabIndex++ %>" value="<%= title %>" />
						</td>
						<td class="addon"><font color="red"><%= invalidTitle ? "!" : "" %></font></td>
						<td class="<%= CssClass.fieldLabel %>"></td>
						<td></td>
						<td class="addon"></td>
					</tr>
					<tr>
						<td class="<%= CssClass.fieldLabel %>"><span class="nw"><%= userView.getFieldLabel(DOCUMENT_CLASS, "qualifiedName", app.getCurrentLocaleAsIndex()) %>:</span></td>
						<td>
							<input type="text" class="valueL" name="qualifiedName" maxlength="50" tabindex="<%= tabIndex++ %>" value="<%= qualifiedName %>" />
						</td>
						<td class="addon"></td>
						<td class="<%= CssClass.fieldLabel %>"></td>
						<td></td>
						<td class="addon"></td>
					</tr>
					<tr>
						<td class="<%= CssClass.fieldLabel %>"><span class="nw"><%= userView.getFieldLabel(DOCUMENT_CLASS, "searchText", app.getCurrentLocaleAsIndex()) %>:</span></td>
						<td>
							<input type="text" class="valueL" name="searchText" maxlength="50" tabindex="<%= tabIndex++ %>" value="<%= searchText %>" />
						</td>
						<td class="addon"></td>
						<td class="<%= CssClass.fieldLabel %>"></td>
						<td></td>
						<td class="addon"></td>
					</tr>
					<tr>
						<td class="<%= CssClass.fieldLabel %>"><span class="nw"><%= userView.getFieldLabel(DOCUMENT_CLASS, "documentNumber", app.getCurrentLocaleAsIndex()) %>:</span></td>
						<td>
							<input type="text" class="valueL" name="documentNumber" maxlength="50" tabindex="<%= tabIndex++ %>" value="<%= documentNumber %>" />
						</td>
						<td class="addon"><font color="red"><%= invalidDocumentNumber ? "!" : "" %></font></td>
						<td class="<%= CssClass.fieldLabel %>"></td>
						<td></td>
						<td class="addon"></td>
					</tr>
					<tr>
						<td class="<%= CssClass.fieldLabel %>"><span class="nw"><%= userView.getFieldLabel(DOCUMENT_CLASS, "author", app.getCurrentLocaleAsIndex()) %>:</span></td>
						<td>
							<input type="text" class="valueL" name="author" maxlength="50" tabindex="<%= tabIndex++ %>" value="<%= author %>" />
						</td>
						<td class="addon"></td>
						<td class="<%= CssClass.fieldLabel %>"></td>
						<td></td>
						<td class="addon"></td>
					</tr>
					<tr>
						<td class="<%= CssClass.fieldLabel %>"><span class="nw"><%= userView.getFieldLabel(DOCUMENT_CLASS, "documentType", app.getCurrentLocaleAsIndex()) %>:</span></td>
						<td >
							<select class="valueL lightUp" name="documentType" tabindex="<%= tabIndex++ %>">
<%
								if (documentTypeCode_longTextsT == null) {
%>
									<option value="0">N/A
<%
								}
								else {
									for(Iterator options = documentTypeCode_longTextsT.entrySet().iterator(); options.hasNext(); ) {
										Map.Entry option = (Map.Entry)options.next();
										short value = Short.parseShort((option.getValue()).toString());
										String selectedModifier = Short.parseShort(documentType) == value ? "selected" : "";
%>
										<option <%= selectedModifier %> value="<%= value %>"><%= (codes.getLongTextByCode(featureDocumentTypeCode, app.getCurrentLocaleAsIndex(), true).get(new Short(value))) %>
<%
									}
								}
%>
							</select>
						</td>
						<td class="addon"></td>
						<td class="<%= CssClass.fieldLabel %>"></td>
						<td></td>
						<td class="addon"></td>
					</tr>
					<tr>
						<td class="<%= CssClass.fieldLabel %>"><span class="nw"><%= userView.getFieldLabel(DOCUMENT_CLASS, "literatureType", app.getCurrentLocaleAsIndex()) %>:</span></td>
						<td >
							<select class="valueL lightUp" name="literatureType" tabindex="<%= tabIndex++ %>">
<%
								if (literatureTypeCode_longTextsT == null) {
%>
									<option value="0">N/A
<%
								}
								else {
									for(Iterator options = literatureTypeCode_longTextsT.entrySet().iterator(); options.hasNext(); ) {
										Map.Entry option = (Map.Entry)options.next();
										short value = Short.parseShort((option.getValue()).toString());
										String selectedModifier = Short.parseShort(literatureType) == value ? "selected" : "";
%>
										<option <%= selectedModifier %> value="<%= value %>"><%= (codes.getLongTextByCode(featureLiteratureTypeCode, app.getCurrentLocaleAsIndex(), true).get(new Short(value))) %>
<%
									}
								}
%>
							</select>
						</td>
						<td class="addon"></td>
						<td class="<%= CssClass.fieldLabel %>"></td>
						<td></td>
						<td class="addon"></td>
					</tr>
				</table>
<%
			}
			if(
				!(object instanceof org.opencrx.kernel.document1.jmi1.DocumentFolder) &&
				!(object instanceof org.opencrx.kernel.document1.jmi1.Document)
			) {
%>
				<div class="<%= CssClass.fieldGroupName %>">&nbsp;</div>			
				<table class="fieldGroup">
					<tr>
						<td class="<%= CssClass.fieldLabel %>"><span class="nw"><%= app.getLabel(DOCUMENTFOLDER_CLASS) %>:</span></td>
<%
						String lookupId = org.opencrx.kernel.backend.Activities.getInstance().getUidAsString();
						Action findDocumentFolderObjectAction = Action.getFindObjectAction(DOCUMENT_CLASS + ":folder", lookupId);
%>
						<td>
							<div class="autocompleterMenu">
								<ul id="<%=CssClass.ssf_nav %>" class="<%=CssClass.ssf_nav %>" onmouseover="sfinit(this);" >
									<li><a href="#"><img border="0" alt="" src="../../images/autocomplete_select.png" /></a>
										<ul onclick="this.style.left='-999em';" onmouseout="this.style.left='';">
											<li class="selected"><a href="#" onclick="javascript:navSelect(this);ac_addObject0.url= './'+getEncodedHRef(['ObjectInspectorServlet', 'event', '40', 'parameter', 'xri*(xri:@openmdx:org.opencrx.kernel.document1/provider/<%= providerName %>/segment/<%= segmentName %>)*referenceName*(folder)*filterByType*(org:opencrx:kernel:document1:DocumentFolder)*filterByFeature*(name)*filterOperator*(IS_LIKE)*orderByFeature*(name)*position*(0)*size*(20)']);return false;"><span>&nbsp;&nbsp;&nbsp;</span>Document Folder / Name</a></li>
											<li><a href="#" onclick="javascript:navSelect(this);ac_addObject0.url= './'+getEncodedHRef(['ObjectInspectorServlet', 'event', '40', 'parameter', 'xri*(xri:@openmdx:org.opencrx.kernel.document1/provider/<%= providerName %>/segment/<%= segmentName %>)*referenceName*(folder)*filterByType*(org:opencrx:kernel:document1:DocumentFolder)*filterByFeature*(description)*filterOperator*(IS_LIKE)*orderByFeature*(description)*position*(0)*size*(20)']);return false;"><span>&nbsp;&nbsp;&nbsp;</span>Document Folder / Description</a></li>
										</ul>
									</li>
								</ul>
							</div>
							<div class="autocompleterInput"><input type="text" class="valueL valueAC" id="documentFolder.Title" name="documentFolder.Title" tabindex="<%= tabIndex++ %>" value="<%= documentFolderTitle != null ? documentFolderTitle : "" %>" /></div>
							<input type="hidden" class="valueLLocked" id="documentFolder" name="documentFolder" readonly value="<%= documentFolder != null ? documentFolder : "" %>" />
							<div class="autocomplete" id="documentFolder.Update" style="display:none;z-index:500;"></div>
							<script type="text/javascript" language="javascript" charset="utf-8">
								ac_addObject0 = new Ajax.Autocompleter(
									'documentFolder.Title',
									'documentFolder.Update',
									'../../ObjectInspectorServlet?event=40&parameter=xri*%28xri%3A%40openmdx%3Aorg.opencrx.kernel.document1%2Fprovider%2F<%= providerName %>%2Fsegment%2F<%= segmentName %>%29*referenceName*%28folder%29*filterByType*%28org%3Aopencrx%3Akernel%3Adocument1%3ADocumentFolder%29*filterByFeature*%28name%29*filterOperator*%28IS_LIKE%29*orderByFeature*%28name%29*position*%280%29*size*%2820%29',
									{
										paramName: 'filtervalues',
										minChars: 0,
										afterUpdateElement: updateXriField
									}
								);
							</script>
						</td>
						<td class="addon">
							<img class="popUpButton" border="0" align="bottom" alt="Click to open ObjectFinder" src="../../images/lookup.gif" onclick="OF.findObject('../../<%= findDocumentFolderObjectAction.getEncodedHRef(requestId) %>', $('documentFolder.Title'), $('documentFolder'), '<%= lookupId %>');" /></div>
						</td>
						<td class="<%= CssClass.fieldLabel %>"></td>
						<td></td>
						<td class="addon"></td>
					</tr>
				</table>
				<div class="<%= CssClass.fieldGroupName %>">&nbsp;</div>			
				<table class="fieldGroup">			
<%
					int ii = 0;
					org.opencrx.security.realm1.cci2.PrincipalGroupQuery principalGroupQuery = (org.opencrx.security.realm1.cci2.PrincipalGroupQuery)pm.newQuery(org.opencrx.security.realm1.jmi1.PrincipalGroup.class);
					principalGroupQuery.forAllDisabled().isFalse();
					principalGroupQuery.orderByName().ascending();
					for(String owningGroup: Arrays.asList(owningGroup00, owningGroup01, owningGroup02)) {
%>				
						<tr>
							<td class="<%= CssClass.fieldLabel %>"><span class="nw"><%= app.getLabel(PRINCIPAL_GROUP_CLASS) %> #<%= ii+1 %>:</span></td>						
							<td>
		  	        			<select class="valueL" name="owningGroup<%= ii %>" id="owningGroup<%= ii %>" tabindex="<%= tabIndex++ %>">
		  	        				<option <%= owningGroup == null || owningGroup.isEmpty() ? "selected" : "" %> value="">N/A
<%
									List<org.opencrx.security.realm1.jmi1.PrincipalGroup> groups =  realm.getPrincipal(principalGroupQuery);
					                for(org.opencrx.security.realm1.jmi1.PrincipalGroup group: groups) {
					                	String selectedModifier = (owningGroup != null) && (owningGroup.compareTo(group.refMofId()) == 0) ? "selected" : "";
%>
										<option <%= selectedModifier %> value="<%= group.refMofId() %>"><%= group.getName() %> [<%= group.getDescription() %>]
<%
									}
%>
								</select>
							</td>
							<td class="addon"></td>
							<td class="<%= CssClass.fieldLabel %>"></td>
							<td></td>
							<td class="addon"></td>					        
				        </tr>
<%
						ii++;
					}
%>		        
				</table>
<%
			}
%>				
			<div class="<%= CssClass.fieldGroupName %>">&nbsp;</div>			
			<table class="fieldGroup">
				<tr>
					<td colspan="3" >
						<br>
						<input type="Submit" name="OK.Button" class="<%= CssClass.btn.toString() %> <%= CssClass.btn_light.toString() %>" tabindex="<%= tabIndex++ %>" value="<%= app.getTexts().getSaveTitle() %>" />
						<input type="Submit" name="Cancel.Button" class="<%= CssClass.btn.toString() %> <%= CssClass.btn_light.toString() %>" tabindex="<%= tabIndex++ %>" value="<%= app.getTexts().getCancelTitle() %>" />
					</td>
					<td colspan="3"></td>
				</tr>
			</table>			
		</td>
	</tr>
</table>
</form>
<%
					}
      				if (successfullyCreated && document != null) {
      					// Go to created document
      					Action nextAction =
      						new Action(
      							SelectObjectAction.EVENT_ID,
      							new Action.Parameter[]{
      								new Action.Parameter(Action.PARAMETER_OBJECTXRI, document.refMofId())
      							},
      							"", 
      							true
      						);
      					response.sendRedirect(
      						request.getContextPath() + "/" + nextAction.getEncodedHRef()
      					);
      				}
      			}
				catch (Exception ex) {
					out.println("<pre><b>!! Failed !!<br><br>The following exception occur:</b><br><br>");
					ex.printStackTrace(new PrintWriter(out));
					out.println("</pre>");
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
