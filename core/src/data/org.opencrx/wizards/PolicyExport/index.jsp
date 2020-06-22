<%@  page contentType="text/html;charset=UTF-8" language="java" pageEncoding="UTF-8" %><%
/*
 * ====================================================================
 * Project:     opencrx, http://www.opencrx.org/
 * Name:        PolicyExport.jsp
 * Description: export policy/roles/privileges/permissions
 * Owner:       CRIXP Corp., Switzerland, http://www.crixp.com
 * ====================================================================
 *
 * This software is published under the BSD license
 * as listed below.
 *
 * Copyright (c) 2013, CRIXP Corp., Switzerland
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
java.net.URL,
java.net.MalformedURLException,
java.io.UnsupportedEncodingException,
org.openmdx.kernel.id.*,
org.opencrx.kernel.portal.*,
org.openmdx.base.accessor.jmi.cci.*,
org.openmdx.base.exception.*,
org.openmdx.portal.servlet.*,
org.openmdx.portal.servlet.attribute.*,
org.openmdx.portal.servlet.component.*,
org.openmdx.portal.servlet.control.*,
org.openmdx.portal.servlet.wizards.*,
org.openmdx.base.naming.*,
org.openmdx.base.query.*,
org.openmdx.kernel.log.*,
org.apache.poi.hssf.usermodel.*,
org.apache.poi.hssf.util.*
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
%>
<!DOCTYPE HTML PUBLIC "-//W3C//DTD HTML 4.01 Transitional//EN" "http://www.w3.org/TR/html4/loose.dtd">
<html>

<head>
  <title>Export Policy --&gt; XLS (Roles/Privileges/Permissions)</title>
  <meta http-equiv="Content-Type" content="text/html; charset=utf-8">
  <meta http-equiv="Content-Type" content="text/html; charset=utf-8">
  <link rel="stylesheet" href="../../js/bootstrap/css/bootstrap.min.css">	
  <link rel="stylesheet" href="../../_style/colors.css">
  <link rel="stylesheet" href="../../_style/n2default.css">
  <link rel="stylesheet" href="../../_style/ssf.css">
  <link rel='shortcut icon' href='../../images/favicon.ico' />
</head>

<body class="ytheme-gray">
<%
	String formAction = "PolicyExport.jsp";
	String sheetName = "openCRX_Policy.xls";
	try {
		
      RefObject_1_0 obj = (RefObject_1_0)pm.getObjectById(new Path(objectXri));
      org.openmdx.security.authorization1.jmi1.Policy policy = (org.openmdx.security.authorization1.jmi1.Policy)obj;
      
      org.opencrx.kernel.home1.jmi1.UserHome currentUserHome = (org.opencrx.kernel.home1.jmi1.UserHome)pm.getObjectById(
    			app.getUserHomeIdentityAsPath()
    		);
      Path objectPath = new Path(currentUserHome.refMofId());
      String providerName = objectPath.get(2);
      String segmentName = objectPath.get(4);
		
			// Generate spreadsheet
			String location = UUIDs.getGenerator().next().toString();
			File f = new File(
			    app.getTempFileName(location, "")
			);
			FileOutputStream os = new FileOutputStream(f);
			
			HSSFWorkbook wb = new HSSFWorkbook();
			HSSFSheet sheetRoles = wb.createSheet("Roles");
			HSSFSheet sheetPrivileges = wb.createSheet("Privileges");

// Generating Row1 (Privileges)
			short PRIVnRow = 0;
			HSSFRow privRow = sheetPrivileges.createRow(PRIVnRow++);
			short nCell = 0;
			
			HSSFCell cell = privRow.createCell(nCell++);
			cell.setCellValue("XRI");
			
			cell = privRow.createCell(nCell++);
			cell.setCellValue("name");
			
			cell = privRow.createCell(nCell++);
			cell.setCellValue("description");
			
			cell = privRow.createCell(nCell++);
			cell.setCellValue("action");
  	  
// Exporting Privileges
			org.openmdx.security.realm1.cci2.PrivilegeQuery privilegeQuery = (org.openmdx.security.realm1.cci2.PrivilegeQuery)pm.newQuery(org.openmdx.security.realm1.jmi1.Privilege.class);
			privilegeQuery.orderByName().ascending();
			for (
				Iterator i = policy.getPrivilege(privilegeQuery).iterator();
				i.hasNext();
			) {
				try {
					org.openmdx.security.realm1.jmi1.Privilege priv = (org.openmdx.security.realm1.jmi1.Privilege)i.next();
					// Generating Row
					privRow = sheetPrivileges.createRow(PRIVnRow++);
					nCell = 0;
					cell = privRow.createCell(nCell++);
					cell.setCellValue(priv.refMofId());
					cell = privRow.createCell(nCell++);
					if (priv.getName() != null) {
						cell.setCellValue(priv.getName());
					}
					cell = privRow.createCell(nCell++);
					if (priv.getDescription() != null) {
						cell.setCellValue(priv.getDescription());
					}
					cell = privRow.createCell(nCell++);
					if (priv.getAction() != null) {
						for (Iterator s = priv.getAction().iterator(); s.hasNext();) {
							cell.setCellValue((String)s.next());								
							cell = privRow.createCell(nCell++);
						}
					}
				} catch (Exception e) {
					new ServiceException(e).log();
				}
			}
  	  
// Generating Row1 (Privileges)
			short ROLEnRow = 0;
			HSSFRow roleRow = sheetRoles.createRow(ROLEnRow++);
			nCell = 0;
			
			cell = roleRow.createCell(nCell++);
			cell.setCellValue("XRI");
			
			cell = roleRow.createCell(nCell++);
			cell.setCellValue("name");
			
			cell = roleRow.createCell(nCell++);
			cell.setCellValue("description");
			
			cell = roleRow.createCell(nCell++);
			cell.setCellValue("disabled");
  	  
			cell = roleRow.createCell(nCell++);
			cell.setCellValue("permission.XRI");
			
			cell = roleRow.createCell(nCell++);
			cell.setCellValue("permission.name");
			
			cell = roleRow.createCell(nCell++);
			cell.setCellValue("permission.description");
			
			cell = roleRow.createCell(nCell++);
			cell.setCellValue("permission.privilege");

			cell = roleRow.createCell(nCell++);
			cell.setCellValue("permission.action");
			
			// Exporting Roles and Permissions
			org.openmdx.security.realm1.cci2.RoleQuery roleQuery = (org.openmdx.security.realm1.cci2.RoleQuery)pm.newQuery(org.openmdx.security.realm1.jmi1.Role.class);
			roleQuery.orderByName().ascending();
			for (
				Iterator i = policy.getRole(roleQuery).iterator();
				i.hasNext();
			) {
				try {
					org.openmdx.security.realm1.jmi1.Role role = (org.openmdx.security.realm1.jmi1.Role)i.next();

					// role
					roleRow = sheetRoles.createRow(ROLEnRow++);
					nCell = 0;
					cell = roleRow.createCell(nCell++);
					cell.setCellValue(role.refMofId());
					cell = roleRow.createCell(nCell++);
					if (role.getName() != null) {
						cell.setCellValue(role.getName());
					}
					cell = roleRow.createCell(nCell++);
					if (role.getDescription() != null) {
						cell.setCellValue(role.getDescription());
					}
					cell = roleRow.createCell(nCell++);
					cell.setCellValue(role.isDisabled());
					
					org.openmdx.security.realm1.cci2.PermissionQuery permissionQuery = (org.openmdx.security.realm1.cci2.PermissionQuery)pm.newQuery(org.openmdx.security.realm1.jmi1.Permission.class);
					permissionQuery.orderByName().ascending();
					for (
							Iterator p = role.getPermission(permissionQuery).iterator();
							p.hasNext();
					) {
							try {
								org.openmdx.security.realm1.jmi1.Permission permission = (org.openmdx.security.realm1.jmi1.Permission)p.next();
								// Generating Row
								roleRow = sheetRoles.createRow(ROLEnRow++);
								nCell = 0;
								cell = roleRow.createCell(nCell++);
								cell.setCellValue("\\");
								cell = roleRow.createCell(nCell++);
								cell = roleRow.createCell(nCell++);
								cell = roleRow.createCell(nCell++);
								
								// permission
								cell = roleRow.createCell(nCell++);
								
								cell.setCellValue(permission.refMofId());
								cell = roleRow.createCell(nCell++);
								if (permission.getName() != null) {
									cell.setCellValue(permission.getName());
								}
								cell = roleRow.createCell(nCell++);
								if (permission.getDescription() != null) {
									cell.setCellValue(permission.getDescription());
								}
								cell = roleRow.createCell(nCell++);
								if (permission.getPrivilege() != null) {
									cell.setCellValue(permission.getPrivilege().refMofId());
								}
								cell = roleRow.createCell(nCell++);
								if (permission.getAction() != null) {
									for (Iterator s = permission.getAction().iterator(); s.hasNext();) {
										cell.setCellValue((String)s.next());								
										cell = roleRow.createCell(nCell++);
									}
								}

							} catch (Exception e) {
								new ServiceException(e).log();
							}
					}
				} catch (Exception e) {
					new ServiceException(e).log();
				}
			}

    	wb.write(os);

      os.flush();
      os.close();

      Action downloadAction =
          new Action(
              Action.EVENT_DOWNLOAD_FROM_LOCATION,
              new Action.Parameter[]{
                  new Action.Parameter(Action.PARAMETER_LOCATION, location),
                  new Action.Parameter(Action.PARAMETER_NAME, sheetName),
                  new Action.Parameter(Action.PARAMETER_MIME_TYPE, "application/vnd.ms-excel")
              },
              app.getTexts().getClickToDownloadText() + " " + sheetName,
              true
      );
      response.sendRedirect(
         request.getContextPath() + "/" + downloadAction.getEncodedHRef(requestId)
      );
%>
  Download report from <a href="../<%= downloadAction.getEncodedHRef(requestId) %>">here</a>
</body>
</html>
<%
	}
	catch (Exception e) {
    ServiceException e0 = new ServiceException(e);
    SysLog.warning("Can not create spreadsheet", "Wizard " + formAction);
    SysLog.warning(e0.getMessage(), e0.getCause());
    // Go back to previous view
    Action nextAction = new ObjectReference(
      (RefObject_1_0)pm.getObjectById(new Path(objectXri)),
      app
    ).getSelectObjectAction();
    response.sendRedirect(
       request.getContextPath() + "/" + nextAction.getEncodedHRef()
    );
  }
  finally {
    if(pm != null) {
    	pm.close();
    }
  }
%>
