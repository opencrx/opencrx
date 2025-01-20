<%@  page contentType="text/html;charset=UTF-8" language="java" pageEncoding="UTF-8" %><%
/*
 * ====================================================================
 * Project:     openCRX/Core, http://www.opencrx.org/
 * Name:        PolicyImport.jsp
 * Description: import policy/roles/privileges/permissions
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
java.math.*,
java.net.URL,
java.net.MalformedURLException,
java.io.UnsupportedEncodingException,
org.openmdx.kernel.id.cci.*,
org.openmdx.kernel.id.*,
org.opencrx.kernel.portal.*,
org.openmdx.base.accessor.jmi.cci.*,
org.openmdx.base.exception.*,
org.openmdx.portal.servlet.*,
org.openmdx.portal.servlet.action.*,
org.openmdx.portal.servlet.attribute.*,
org.openmdx.portal.servlet.component.*,
org.openmdx.portal.servlet.control.*,
org.openmdx.portal.servlet.wizards.*,
org.openmdx.base.naming.*,
org.openmdx.base.query.*,
org.openmdx.kernel.log.*,
org.openmdx.uses.org.apache.commons.fileupload.*,
org.apache.poi.hssf.usermodel.*,
org.apache.poi.ss.usermodel.*,
org.apache.poi.hssf.util.*,
org.apache.poi.poifs.filesystem.POIFSFileSystem
" %>
<%!
	public org.openmdx.security.realm1.jmi1.Permission findPermission(
		String name, org.openmdx.security.realm1.jmi1.Role role,
		javax.jdo.PersistenceManager pm
	) {
		org.openmdx.security.realm1.jmi1.Permission permission = null;
		try {
			org.openmdx.security.realm1.cci2.PermissionQuery permissionQuery = (org.openmdx.security.realm1.cci2.PermissionQuery) pm.newQuery(org.openmdx.security.realm1.jmi1.Permission.class);
			permissionQuery.name().equalTo(name);
			List<org.openmdx.security.realm1.jmi1.Permission> permissions = role.getPermission(permissionQuery);
			if (!permissions.isEmpty()) {
				permission = permissions.iterator().next();
			}
		} catch (Exception e) {
		}
		return permission;
	}

	public org.openmdx.security.realm1.jmi1.Permission createOrUpdatePermission(
		String id,
		String name,
		String description,
		Set<String> action,
		org.openmdx.security.realm1.jmi1.Role role,
		org.openmdx.security.realm1.jmi1.Privilege privilege,
		javax.jdo.PersistenceManager pm
	) {
		org.openmdx.security.realm1.jmi1.Permission permission = findPermission(name, role, pm);
		try {
			pm.currentTransaction().begin();
			if (permission == null) {
				// create privilege
				permission = pm.newInstance(org.openmdx.security.realm1.jmi1.Permission.class);
				permission.setName(name);
				permission.setAction(action);
				permission.setPrivilege(privilege);
				role.addPermission(id, permission);
			} else {
				permission.setAction(action);
				permission.setPrivilege(privilege);
				permission.setDescription(description);
			}
			pm.currentTransaction().commit();
		} catch (Exception e) {
			new ServiceException(e).log();
			try {
				pm.currentTransaction().rollback();
			} catch (Exception e0) {
			}
		}
		return permission;
	}

	public org.openmdx.security.realm1.jmi1.Privilege findPrivilege(
		String name,
		org.openmdx.security.authorization1.jmi1.Policy policy,
		javax.jdo.PersistenceManager pm
	) {
		org.openmdx.security.realm1.jmi1.Privilege privilege = null;
		try {
			org.openmdx.security.realm1.cci2.PrivilegeQuery privilegeQuery = (org.openmdx.security.realm1.cci2.PrivilegeQuery) pm.newQuery(org.openmdx.security.realm1.jmi1.Privilege.class);
			privilegeQuery.name().equalTo(name);
			List<org.openmdx.security.realm1.jmi1.Privilege> privileges = policy.getPrivilege(privilegeQuery);
			if (!privileges.isEmpty()) {
				privilege = privileges.iterator().next();
			}
		} catch (Exception e) {
		}
		return privilege;
	}

	public org.openmdx.security.realm1.jmi1.Privilege createOrUpdatePrivilege(
		String id,
		String name,
		String description,
		Set<String> action,
		org.openmdx.security.authorization1.jmi1.Policy policy,
		javax.jdo.PersistenceManager pm
	) {
		org.openmdx.security.realm1.jmi1.Privilege privilege = findPrivilege(name, policy, pm);
		try {
			pm.currentTransaction().begin();
			if (privilege == null) {
				// create privilege
				privilege = pm.newInstance(org.openmdx.security.realm1.jmi1.Privilege.class);
				privilege.setName(name);
				privilege.setDescription(description);
				privilege.setAction(action);
				policy.addPrivilege(
					id,
					privilege
				);
			} else {
				privilege.setDescription(description);
				privilege.setAction(action);
			}
			pm.currentTransaction().commit();
		} catch (Exception e) {
			new ServiceException(e).log();
			try {
				pm.currentTransaction().rollback();
			} catch (Exception e0) {
			}
		}
		return privilege;
	}

	public org.openmdx.security.realm1.jmi1.Role findRole(
		String name,
		org.openmdx.security.authorization1.jmi1.Policy policy,
		javax.jdo.PersistenceManager pm
	) {
		org.openmdx.security.realm1.jmi1.Role role = null;
		try {
			org.openmdx.security.realm1.cci2.RoleQuery roleQuery = (org.openmdx.security.realm1.cci2.RoleQuery) pm.newQuery(org.openmdx.security.realm1.jmi1.Role.class);
			roleQuery.name().equalTo(name);
			List<org.openmdx.security.realm1.jmi1.Role> roles = policy.getRole(roleQuery);
			if (!roles.isEmpty()) {
				role = roles.iterator().next();
			}
		} catch (Exception e) {
		}
		return role;
	}

	public org.openmdx.security.realm1.jmi1.Role createOrUpdateRole(
		String id,
		String name,
		String description,
		boolean disabled,
		org.openmdx.security.authorization1.jmi1.Policy policy,
		javax.jdo.PersistenceManager pm
	) {
		org.openmdx.security.realm1.jmi1.Role role = findRole(name, policy, pm);
		try {
			pm.currentTransaction().begin();
			if (role == null) {
				// create role
				role = pm.newInstance(org.openmdx.security.realm1.jmi1.Role.class);
				role.setName(name);
				role.setDescription(description);
				role.setDisabled(disabled);
				policy.addRole(id, role);
			} else {
				role.setDescription(description);
				role.setDisabled(disabled);
			}
			pm.currentTransaction().commit();
		} catch (Exception e) {
			new ServiceException(e).log();
			try {
				pm.currentTransaction().rollback();
			} catch (Exception e0) {
			}
		}
		return role;
	}
%>
<%
  request.setCharacterEncoding("UTF-8");
	ApplicationContext app = (ApplicationContext)session.getAttribute(WebKeys.APPLICATION_KEY);
	ViewsCache viewsCache = (ViewsCache)session.getAttribute(WebKeys.VIEW_CACHE_KEY_SHOW);
	if (app == null) {
		System.out.println("aborting... (ApplicationContext == null)");
	    response.sendRedirect(
	       request.getContextPath() + "/" + WebKeys.SERVLET_NAME
	    );
		return;
  	}
	Map parameterMap = request.getParameterMap();
  	if(FileUpload.isMultipartContent(request)) {
		parameterMap = new HashMap();
    	DiskFileUpload upload = new DiskFileUpload();
    	upload.setHeaderEncoding("UTF-8");
		try {
        List items = upload.parseRequest(
          request,
          200, // in-memory threshold. Content for fields larger than threshold is written to disk
          50000000, // max request size [overall limit]
          app.getTempDirectory().getPath()
        );
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
	final String formAction = "PolicyImport.jsp";
%>
<!DOCTYPE HTML PUBLIC "-//W3C//DTD HTML 4.01 Transitional//EN" "http://www.w3.org/TR/html4/loose.dtd">
<html>
<head>
	<title>Import Policy from XLS (Roles/Privileges/Permissions)</title>
	<meta http-equiv="Content-Type" content="text/html; charset=utf-8">
	<meta http-equiv="Content-Type" content="text/html; charset=utf-8">
	<link rel="stylesheet" href="../../js/bootstrap/css/bootstrap.min.css">	
	<link rel="stylesheet" href="../../_style/colors.css">
	<link rel="stylesheet" href="../../_style/n2default.css">
	<link rel="stylesheet" href="../../_style/ssf.css">
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
    	<div id="content" style="padding:20px 0.5em 0px 0.5em;">
      <div id="etitle" style="height:20px;">
         Import Excel File with openCRX Policy (Roles/Privileges/Permissions)<br>
      </div>
      <br>

<%
    // Admin.java createPolicy
    NumberFormat priceFormatter = new DecimalFormat("#,##0.00000");
    NumberFormat formatter = new DecimalFormat("0");
		final String UPLOAD_FILE_FIELD_NAME = "uploadFile";
		try {
			boolean actionOk = parameterMap.get("OK.Button") != null;
			boolean actionCancel = parameterMap.get("Cancel.Button") != null;
			boolean continueToExit = false;

			String[] descriptions = (String[])parameterMap.get("description");
			String description = (descriptions == null) || (descriptions.length == 0) ? "" : descriptions[0];

			//System.out.println("XRI=" + objectXri);
			String location = app.getTempFileName(UPLOAD_FILE_FIELD_NAME, "");

			// Get data package. This is the JMI root package to handle
			// openCRX object requests

      RefObject_1_0 obj = (RefObject_1_0)pm.getObjectById(new Path(objectXri));
      org.openmdx.security.authorization1.jmi1.Policy policy = (org.openmdx.security.authorization1.jmi1.Policy)obj;
      
      org.opencrx.kernel.home1.jmi1.UserHome currentUserHome = (org.opencrx.kernel.home1.jmi1.UserHome)pm.getObjectById(
    			app.getUserHomeIdentityAsPath()
    		);
      Path objectPath = new Path(currentUserHome.refMofId());
      String providerName = objectPath.getSegment(2).toString();
      String segmentName = objectPath.getSegment(4).toString();
      boolean currentUserIsAdmin =
				app.getCurrentUserRole().equals(org.opencrx.kernel.generic.SecurityKeys.ADMIN_PRINCIPAL + org.opencrx.kernel.generic.SecurityKeys.ID_SEPARATOR + segmentName + "@" + segmentName);

			if(actionCancel || (objectXri == null) || (!currentUserIsAdmin)) {
        Action nextAction = new ObjectReference(
          (RefObject_1_0)pm.getObjectById(new Path(objectXri)),
          app
        ).getSelectObjectAction();
 				continueToExit = true;
			  if (actionCancel) {
  				response.sendRedirect(
  					request.getContextPath() + "/" + nextAction.getEncodedHRef()
  				);
  		  } else {
  		    String errorMessage = "Cannot upload file (permissions missing?)";
  		    if (!currentUserIsAdmin) {errorMessage = "Wizard disabled or missing permissions (must be Administrator)!";}
%>
          <br />
          <br />
          <span style="color:red;"><b><u>Warning:</u> <%= errorMessage %></b></span>
          <br />
          <br />
          <input type="Submit" name="Cancel.Button" class="<%= CssClass.btn.toString() %> <%= CssClass.btn_light.toString() %>" tabindex="1" value="<%= app.getTexts().getContinueTitle() %>" onClick="javascript:location='<%= request.getContextPath() + "/" + nextAction.getEncodedHRef() %>';" />
          <br />
          <br />
          <hr>
<%
  		  }
			}
			else if(actionOk) {
			  int privilegeLinesRead = 0;
			  int privilegeCreated = 0;
			  int privilegeUpdated = 0;
			  int roleLinesRead = 0;
			  int roleCreated = 0;
			  int roleUpdated = 0;
			  int permissionLinesRead = 0;
			  int permissionCreated = 0;
			  int permissionUpdated = 0;

			  if(
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
              // verify whether File exists
              POIFSFileSystem fs = null;
              try {
                fs = new POIFSFileSystem(new FileInputStream(location));
              } catch (Exception e) {}

          		if(currentUserIsAdmin && actionOk && (fs != null)) {
         				continueToExit = true;

                try {
                  HSSFWorkbook workbook = new HSSFWorkbook(fs);
                  HSSFSheet sheetRoles = null;
                  HSSFSheet sheetPrivileges = null;
                  HSSFSheet tempSheet = workbook.getSheetAt(0);
                  if (tempSheet != null && tempSheet.getSheetName().compareTo("Roles") == 0) {
                	  sheetRoles =  workbook.getSheetAt(0);
                  } else if (tempSheet != null && tempSheet.getSheetName().compareTo("Privileges") == 0) {
                	  sheetPrivileges =  workbook.getSheetAt(0);
                  }
                  tempSheet = workbook.getSheetAt(1);
                  if (tempSheet != null && tempSheet.getSheetName().compareTo("Roles") == 0) {
                	  sheetRoles =  workbook.getSheetAt(1);
                  } else if (tempSheet != null && tempSheet.getSheetName().compareTo("Privileges") == 0) {
                	  sheetPrivileges =  workbook.getSheetAt(1);
                  }
				  // read Privileges
				  Map privilegeMapper = new TreeMap(); // (XRIexport, XRIsystem)
                  Iterator rows = sheetPrivileges.rowIterator();
                  int nRow = 0;
                  HSSFRow row = null;
                  if (rows.hasNext()) {
                    row = (HSSFRow) rows.next(); // skip row#1 containing headers
                  }
                  while (rows.hasNext()) {
                    nRow += 1;
                    privilegeLinesRead += 1;
                    row = (HSSFRow) rows.next();
                    String XRI = null;
                    String name = null;
                    String desc = null;
                    String act = null;
                    short nCell = 0;
                    try {
                      HSSFCell cell = (HSSFCell)row.getCell(nCell++);
                      if (cell != null && cell.getCellType() == CellType.STRING) {
                        XRI = cell.getStringCellValue().trim();
                      }
                      cell = (HSSFCell)row.getCell(nCell++);
                      if (cell != null && cell.getCellType() == CellType.STRING) {
                        name = cell.getStringCellValue().trim();
                      }
                      cell = (HSSFCell)row.getCell(nCell++);
                      if (cell != null && cell.getCellType() == CellType.STRING) {
                        desc = cell.getStringCellValue().trim();
                      }
                      Set<String> action = new HashSet<String>();
                      cell = (HSSFCell)row.getCell(nCell++);
                      while (cell != null && cell.getCellType() == CellType.STRING) {
                    	  action.add(cell.getStringCellValue().trim());
                        cell = (HSSFCell)row.getCell(nCell++);
                      }
                      if(name != null) {
                    	  boolean exists = findPrivilege(name, policy, pm) != null;
                    	  org.openmdx.security.realm1.jmi1.Privilege privilege = createOrUpdatePrivilege(
                    		  XRI == null || XRI.isEmpty()
                    		  	  ? org.opencrx.kernel.utils.Utils.getUidAsString() 
                    		  	  : new Path(XRI).getLastSegment().toString(),
                    		  name,
                    		  desc,
                    		  action,
                    		  policy,
                    		  pm
                    		);
                    	  if (privilege != null) {
                    		  if (XRI != null) {
                    			  privilegeMapper.put(XRI, privilege.refMofId());
                    		  }
							  %> <%= exists ? "updated" : "created" %> privilege <%= name %><br><%
							  if (exists) {
								  privilegeUpdated++;
							  } else {
								  privilegeCreated++;
							  }
                    	  }
                      }
                    } catch (Exception e) {
                    	new ServiceException(e).log();
                    }
                  }
				  // read Roles and permissions                  
                  rows = sheetRoles.rowIterator();
                  nRow = 0;
                  row = null;
                  if (rows.hasNext()) {
                    row = (HSSFRow) rows.next(); // skip row#1 containing headers
                  }
                  org.openmdx.security.realm1.jmi1.Role role = null;
                  while (rows.hasNext()) {
                    nRow += 1;
                    roleLinesRead += 1;
                    row = (HSSFRow) rows.next();
                    String XRI = null;
                    String name = null;
                    String desc = null;
                    boolean disabled = false;
                    String privXRI = null;
                    String act = null;
                    short nCell = 0;
                    try {
                      HSSFCell cell = (HSSFCell)row.getCell(nCell++);
                      if (cell != null && cell.getCellType() == CellType.STRING) {
                        XRI = cell.getStringCellValue().trim();
                      }
                      if (XRI != null && XRI.compareTo("\\") == 0) {
                    	  	permissionLinesRead += 1;
                    	  	roleLinesRead -= 1;
	                  	  	// permission
                    	  	if (role != null) {
		                    	  	nCell += 3;
		                          cell = (HSSFCell)row.getCell(nCell++);
		                          if (cell != null && cell.getCellType() == CellType.STRING) {
		                            XRI = cell.getStringCellValue().trim();
		                          }
				                      cell = (HSSFCell)row.getCell(nCell++);
				                      if (cell != null && cell.getCellType() == CellType.STRING) {
				                        name = cell.getStringCellValue().trim();
				                      }
				                      cell = (HSSFCell)row.getCell(nCell++);
				                      if (cell != null && cell.getCellType() == CellType.STRING) {
				                        desc = cell.getStringCellValue().trim();
				                      }
		                          cell = (HSSFCell)row.getCell(nCell++);
		                          if (cell != null && cell.getCellType() == CellType.STRING) {
		                            privXRI = cell.getStringCellValue().trim();
		                          }
		                          Set<String> action = new HashSet<String>();
		                          cell = (HSSFCell)row.getCell(nCell++);
		                          while (cell != null && cell.getCellType() == CellType.STRING) {
		                        	  action.add(cell.getStringCellValue().trim());
		                            cell = (HSSFCell)row.getCell(nCell++);
		                          }
		                          org.openmdx.security.realm1.jmi1.Privilege privilege = null;
		                          try {
		                          		String privSystemXRI = (String)privilegeMapper.get(privXRI);
		                          		privilege = (org.openmdx.security.realm1.jmi1.Privilege)pm.getObjectById(new Path(privSystemXRI));
		                          } catch (Exception e) {
		                        	  	new ServiceException(e).log();
		                          }
		                          if (name != null) {
		                        	  boolean exists = findPermission(name, role, pm) != null;
		                        	  org.openmdx.security.realm1.jmi1.Permission permission = createOrUpdatePermission(
	                            		  XRI == null || XRI.isEmpty()
		                        		  	  ? org.opencrx.kernel.utils.Utils.getUidAsString() 
		                        		  	  : new Path(XRI).getLastSegment().toString(),
	                        			  name,
	                        			  desc,
	                        			  action,
	                        			  role,
	                        			  privilege,
	                        			  pm
		                        		);
		                        	  if (permission != null) {
										  %> <%= exists ? "updated" : "created" %> permission <%= name %><br><%
										  if (exists) {
											  permissionUpdated++;
										  } else {
											  permissionCreated++;
										  }
		                        	  }
		                          }
                    	  	}             
	                    } else {
	                  	  	// role
	                  	  	role = null;
		                      cell = (HSSFCell)row.getCell(nCell++);
		                      if (cell != null && cell.getCellType() == CellType.STRING) {
		                        name = cell.getStringCellValue().trim();
		                      }
		                      cell = (HSSFCell)row.getCell(nCell++);
		                      if (cell != null && cell.getCellType() == CellType.STRING) {
		                        desc = cell.getStringCellValue().trim();
		                      }
		                      cell = (HSSFCell)row.getCell(nCell++);
		                      if (cell != null && cell.getCellType() == CellType.STRING) {
		                    	  disabled = cell.getBooleanCellValue();
		                      }
		                      if (name != null) {
		                    	  boolean exists = findRole(name, policy, pm) != null;
		                    	  role = createOrUpdateRole(
	                        		  XRI == null || XRI.isEmpty()
	                    		  	  ? org.opencrx.kernel.utils.Utils.getUidAsString() 
	                    		  	  : new Path(XRI).getLastSegment().toString(),
		                    			  name,
		                    			  desc,
		                    			  disabled,
		                    			  policy,
		                    			  pm
		                    		);
		                    	  if (role != null) {
									  %> <%= exists ? "updated" : "created" %> role <%= name %><br><%
									  if (exists) {
										  roleUpdated++;
									  } else {
										  roleCreated++;
									  }
		                    	  }
		                      }
	                    }
                    } catch (Exception e) {
                    	new ServiceException(e).log();
                    }
                  }
                } catch (Exception e) {
                  System.out.println("Exception in xls " + e.getClass());
%>
                  <span style="color:red;"><b><u>Warning:</u> Error reading Excel File! (<%= "class=" + e.getClass() %>)</b></span>
                  <br />
                  <br />
<%
                }
              }
			  new File(location).delete();
              // Go back to previous view
              Action nextAction =
                new Action(
                  SelectObjectAction.EVENT_ID,
                  new Action.Parameter[]{
                    new Action.Parameter(Action.PARAMETER_OBJECTXRI, objectXri)
                    },
                  "", true
          	  );
%>
              <br />
              <br />
              <table><tr><td>
								<table class="gridTableFull">
									<tr class="gridTableHeaderFull">
	              		<td>Object</td>
	              		<td>#lines read</td>
	              		<td>#created</td>
	              		<td>#updated</td>
	              	</tr>
									<tr class="gridTableRowFull">
	              		<td>Roles</td>
	              		<td><%= roleLinesRead %></td>
	              		<td><%= roleCreated %></td>
	              		<td><%= roleUpdated %></td>
	              	</tr>
									<tr class="gridTableRowFull">
	              		<td>Permissions</td>
	              		<td><%= permissionLinesRead %></td>
	              		<td><%= permissionCreated %></td>
	              		<td><%= permissionUpdated %></td>
	              	</tr>
									<tr class="gridTableRowFull">
	              		<td>Privilege</td>
	              		<td><%= privilegeLinesRead %></td>
	              		<td><%= privilegeCreated %></td>
	              		<td><%= privilegeUpdated %></td>
	              	</tr>
	              </table>
	            </td></tr></table>
              <br />
              <br />
              <input type="Submit" name="Cancel.Button" class="<%= CssClass.btn.toString() %> <%= CssClass.btn_light.toString() %>" tabindex="1" value="<%= app.getTexts().getContinueTitle() %>" onClick="javascript:location='<%= request.getContextPath() + "/" + nextAction.getEncodedHRef() %>';" />
              <br />
              <br />
              <hr>
<%
						}
						catch(Exception e) {}
					}
				}
			  else {
			  }
			}
			else {
				File uploadFile = new File(location);
				System.out.println("Import: file " + location + " either does not exist or has size 0: exists=" + uploadFile.exists() + "; length=" + uploadFile.length());
			}
			if (!continueToExit) {
%>
<form name="UploadMedia" enctype="multipart/form-data" accept-charset="UTF-8" method="POST" action="<%= "../.." + request.getServletPath() %>">
<input type="hidden" class="valueL" name="xri" value="<%= objectXri %>" />
<input type="hidden" name="<%= Action.PARAMETER_REQUEST_ID %>" value="<%= requestId %>" />
<table cellspacing="8" class="tableLayout">
  <tr>
    <td class="cellObject">
      <noscript>
        <div class="panelJSWarning" style="display: block;">
          <a href="../../helpJsCookie.html" target="_blank"><img class="popUpButton" src="../../images/help.gif" width="16" height="16" border="0" onclick="javascript:void(window.open('helpJsCookie.html', 'Help', 'fullscreen=no,toolbar=no,status=no,menubar=no,scrollbars=yes,resizable=yes,directories=no,location=no,width=400'));" alt="" /></a> <%= texts.getPageRequiresScriptText() %>
        </div>
      </noscript>

      <div class="panel" id="panelObj0" style="display:block;overflow:visible;">
        <div class="fieldGroupName">
          <span style="font-size:9px;">Hint: Row #1 is skipped (labels)</span><br>
          <span style="font-size:9px;">Sheet 'Roles': XRI, name, description, disabled, permission.XRI, permission.name, permission.description, permission.privilege, permission.action</span><br>
          <span style="font-size:9px;">Sheet 'Privileges': XRI, name, description, action</span><br>
        </div>
	      <table class="fieldGroup">
    			<tr>
    				<td class="<%= CssClass.fieldLabel %>"><span class="nw">File:</span></td>
    				<td >
    					<input type="file" class="valueL" size="100" name="<%= UPLOAD_FILE_FIELD_NAME %>" tabindex="500" />
    				</td>
    				<td class="addon" >
    			</tr>
    			<tr>
	          <td class="<%= CssClass.fieldLabel %>" colspan="3">
	          	<input type="Submit" name="OK.Button" class="<%= CssClass.btn.toString() %> <%= CssClass.btn_light.toString() %>" tabindex="1000" value="Import" />
      			  <input type="Submit" name="Cancel.Button" class="<%= CssClass.btn.toString() %> <%= CssClass.btn_light.toString() %>" tabindex="1010" value="<%= app.getTexts().getCancelTitle() %>" />
	          </td>
	          <td></td>
	          <td></td>
	          <td></td>
	        </tr>
	      </table>
      </div>
  	</td>
  </tr>
</table>
</form>
<%
      }
    }
    catch (Exception ex) {
      Action nextAction = new ObjectReference(
        (RefObject_1_0)pm.getObjectById(new Path(objectXri)),
        app
      ).getSelectObjectAction();
%>
      <br />
      <br />
      <span style="color:red;"><b><u>Warning:</u> cannot upload file (no permission?)</b></span>
      <br />
      <br />
      <input type="Submit" name="Continue.Button" class="<%= CssClass.btn.toString() %> <%= CssClass.btn.toString() %>" tabindex="1" value="Continue" onClick="javascript:location='<%= request.getContextPath() + "/" + nextAction.getEncodedHRef() %>';" />
      <br />
      <br />
      <hr>
<%
	      ServiceException e0 = new ServiceException(ex);
	      e0.log();
	      out.println("<p><b>!! Failed !!<br><br>The following exception(s) occured:</b><br><br><pre>");
	      PrintWriter pw = new PrintWriter(out);
	      e0.printStackTrace(pw);
	      out.println("</pre></p>");
    }
    finally {
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
