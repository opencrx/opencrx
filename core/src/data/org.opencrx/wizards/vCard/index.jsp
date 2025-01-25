<%@  page contentType="text/html;charset=UTF-8" language="java" pageEncoding="UTF-8" %><%
/*
 * ====================================================================
 * Project:     opencrx, http://www.opencrx.org/
 * Description: create vCard(s)
 * Owner:       the original authors.
 * ====================================================================
 *
 * This software is published under the BSD license
 * as listed below.
 *
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
java.util.zip.*,
java.io.*,
java.text.*,
java.math.*,
java.net.*,
java.sql.*,
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
org.openmdx.base.text.conversion.*,
org.openmdx.kernel.log.*
" %>
<%!

  public String makeFileName(
    String fn
  ) {
    String filename = (fn == null ? "---" : fn);
    filename = filename.replaceAll(", ", " ");
    filename = filename.replaceAll(" +", "_");
    filename = filename.replaceAll("ü", "u");
    filename = filename.replaceAll("Ü", "U");
    filename = filename.replaceAll("ä", "a");
    filename = filename.replaceAll("Ä", "A");
    filename = filename.replaceAll("ö", "o");
    filename = filename.replaceAll("Ö", "O");
    filename = filename.replaceAll("é", "e");
    filename = filename.replaceAll("è", "e");
    filename = filename.replaceAll("ê", "e");
    filename = filename.replaceAll("á", "a");
    filename = filename.replaceAll("à", "a");
    filename = filename.replaceAll("â", "a");
    filename = filename.replaceAll("ô", "o");
    filename = filename.replaceAll("&", "_");
    return filename;
  }

%>
<%
	request.setCharacterEncoding("UTF-8");
	ApplicationContext app = (ApplicationContext)session.getAttribute("ObjectInspectorServlet.ApplicationContext");
	ViewsCache viewsCache = (ViewsCache)session.getAttribute(WebKeys.VIEW_CACHE_KEY_SHOW);
	String requestId =  request.getParameter(Action.PARAMETER_REQUEST_ID);
	String objectXri = request.getParameter(Action.PARAMETER_OBJECTXRI);
 	if(objectXri == null || app == null || viewsCache.getView(requestId) == null) {
      response.sendRedirect(
         request.getContextPath() + "/" + WebKeys.SERVLET_NAME
      );
      return;
	}
	Texts_1_0 texts = app.getTexts();
	javax.jdo.PersistenceManager pm = app.getNewPmData();
%>
<!DOCTYPE HTML PUBLIC "-//W3C//DTD HTML 4.01 Transitional//EN" "http://www.w3.org/TR/html4/loose.dtd">
<html>
<head>
	<title><%= app.getApplicationName() %> - vCard</title>
	<meta http-equiv="Content-Type" content="text/html; charset=utf-8">
	<meta http-equiv="Content-Type" content="text/html; charset=utf-8">
	<script language="javascript" type="text/javascript" src="../../js/prototype.js"></script>
	<link rel="stylesheet" href="../../js/bootstrap/css/bootstrap.min.css">	
	<link rel="stylesheet" href="../../_style/colors.css">
	<link rel="stylesheet" href="../../_style/n2default.css">
	<link rel="stylesheet" href="../../_style/ssf.css">
	<link href="../../_style/calendar-small.css" rel="stylesheet" type="text/css">
	<link rel='shortcut icon' href='../../images/favicon.ico' />
</head>
<%
   final String FORM_ACTION = "vCard.jsp";
   final String MIMETYPE_VCARD = "text/x-vcard";
   final String MIMETYPE_ZIP = "application/zip";
   final String MIMETYPE_TEXT = "text/plain";

   final String ENCODING_UTF8 = "UTF-8";
   final String ENCODING_ISO = "ISO-8859-1";
   String defaultFileEncoding = ENCODING_UTF8;
   if (request.getHeader("user-agent").toUpperCase().contains("WIN")) {
       // client browser runs on Windows
       defaultFileEncoding = ENCODING_ISO;
   }

   NumberFormat formatter6 = new DecimalFormat("000000");

   boolean multipleFiles = false;

   try {
       RefObject_1_0 obj = (RefObject_1_0)pm.getObjectById(new Path(objectXri));

       Path objPath = new Path(obj.refMofId());
       String providerName = objPath.get(2);
       String segmentName = objPath.get(4);

%>
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
              <form name="vCard" accept-charset="UTF-8" method="POST" action="<%= "../.." + request.getServletPath() %>">
                <div style="background-color:#F4F4F4;border:1px solid #EBEBEB;padding:10px;margin-top:15px;">
                  <h1>vCard - <%= (new ObjectReference(obj, app)).getLabel() %></h1>
                  <div style="background-color:#FFFFBB;margin:5px 0px;padding:5px;"><i><%= (new ObjectReference(obj, app)).getTitle() %></i></div>
                  <INPUT type="hidden" name="<%= Action.PARAMETER_OBJECTXRI %>" value="<%= objectXri %>" />
                  <input type="hidden" name="<%= Action.PARAMETER_REQUEST_ID %>" value="<%= requestId %>" />
<%
   // Prepare
    org.opencrx.kernel.account1.jmi1.Account account = null;
    String location = UUIDConversion.toUID(UUIDs.newUUID());
    String filename = "---";
    String downloadFileName = "---";
    FileOutputStream fileos = null;
    ZipOutputStream zipos = null;

    boolean isAccount = false;
    boolean isAccountFilterGlobal = false;
    Iterator i = null;
    if (obj instanceof org.opencrx.kernel.account1.jmi1.Group) {
      multipleFiles = true;
      downloadFileName = ((org.opencrx.kernel.account1.jmi1.Group)obj).getName();
      i = ((org.opencrx.kernel.account1.jmi1.Group)obj).getMember().iterator();
      // prepare zip file to be sent to browser
    }
    if (obj instanceof org.opencrx.kernel.account1.jmi1.AccountFilterGlobal) {
      multipleFiles = true;
      downloadFileName = ((org.opencrx.kernel.account1.jmi1.AccountFilterGlobal)obj).getName();
      i = ((org.opencrx.kernel.account1.jmi1.AccountFilterGlobal)obj).getFilteredAccount().iterator();
      // prepare zip file to be sent to browser
      isAccountFilterGlobal = true;
    }
    if (
        (obj instanceof org.opencrx.kernel.account1.jmi1.Contact) ||
        (obj instanceof org.opencrx.kernel.account1.jmi1.LegalEntity) ||
        (obj instanceof org.opencrx.kernel.account1.jmi1.UnspecifiedAccount)
    ) {
      isAccount = true;
      account = (org.opencrx.kernel.account1.jmi1.Account)obj;
      downloadFileName = account.getFullName() + "_encoding_" + defaultFileEncoding;
    }

    int counter = 0;
    while (
      ((i != null) && i.hasNext()) ||
      (isAccount && (account != null))
    ) {
      if (!isAccount) {
        if (isAccountFilterGlobal) {
          account = (org.opencrx.kernel.account1.jmi1.Account)i.next();
        } else {
          // Group
          org.opencrx.kernel.account1.jmi1.Member member =
             (org.opencrx.kernel.account1.jmi1.Member)i.next();
          if ((member.isDisabled() != null) && (member.isDisabled().booleanValue())) {
            // skip disabled members
            continue;
          }
          else {
            account = member.getAccount();
          }
          if ((account != null) && (account.isDisabled() != null) && (account.isDisabled().booleanValue())) {
            // skip disabled accounts
            continue;
          }
        }
      }

      if(account != null) {
        counter++;

        filename =
             providerName + "_"
             + segmentName + "_"
             + (formatter6.format(counter)) + "_"
             + account.getFullName() + "_(encoding_" + defaultFileEncoding + ")";
        // note: zip encode cannot handle file names with special chars
        filename = org.opencrx.kernel.utils.Utils.toFilename(makeFileName(filename)) + ".vcf";
        filename = java.net.URLEncoder.encode(filename, "UTF-8");

        if (fileos == null) {
          fileos = new FileOutputStream(app.getTempFileName(location, ""));
        }

        if (multipleFiles) {
          if (zipos == null) {
            zipos = new ZipOutputStream(fileos);
          }
          // Add vCard to ZIP file
          try {
			String vcard = account.getVcard();
			if((vcard != null) && (vcard.indexOf("BEGIN:VCARD") >= 0)) {
               	ByteArrayOutputStream bos = new ByteArrayOutputStream();
               	PrintWriter p = new PrintWriter(new OutputStreamWriter(bos, "UTF-8"));
            	org.opencrx.application.uses.ezvcard.VCard vCard = org.opencrx.application.uses.ezvcard.Ezvcard.parse(vcard).first();
            	if(vCard.getPhotos().isEmpty()) {
            		org.opencrx.application.uses.ezvcard.property.Photo photo = org.opencrx.application.carddav.AccountResource.getPhoto(account);
            		if(photo != null) {
            			vCard.addPhoto(photo);
            		}
                }
            	try {
            		org.opencrx.application.uses.ezvcard.Ezvcard.write(vCard).version(org.opencrx.application.uses.ezvcard.VCardVersion.V3_0).go(p);
            	} catch(Exception ignore) {}
				p.close();
				zipos.putNextEntry(new ZipEntry(filename));
				zipos.write(bos.toByteArray());
			}
            zipos.closeEntry();
          }
          catch (Exception e) {
            try {
              zipos.closeEntry();
            }
            catch (Exception ex) {}
            new ServiceException(e).log();
          }
        } else {
          // create vCard file
          if (account.getVcard() != null) {
            //fileos.write(account.getVcard().getBytes("UTF-8"));
            fileos.write(account.getVcard().getBytes(defaultFileEncoding));
          }
          fileos.flush();
          fileos.close();
        }
      }
      account = null; /* ensure termination if isAccount==true !!! */

    } /* while */

    if(location != null) {
      downloadFileName = makeFileName(downloadFileName);
      String mimeType = MIMETYPE_VCARD;
      if (multipleFiles) {
        zipos.finish();
        zipos.close();
        // determine user-agent because IE doesn't handle application/zip properly
        String userAgent = request.getHeader("User-Agent");
        mimeType = MIMETYPE_ZIP;
        if ((userAgent != null) && (userAgent.indexOf("IE") >=0)) {
           mimeType = MIMETYPE_TEXT;
        }
        downloadFileName += ".zip";
      } else {
        downloadFileName += ".vcf";
      }
      downloadFileName = java.net.URLEncoder.encode(downloadFileName, "UTF-8");

      Action downloadAction = null;
      downloadAction =
        new Action(
          Action.EVENT_DOWNLOAD_FROM_LOCATION,
          new Action.Parameter[]{
            new Action.Parameter(Action.PARAMETER_LOCATION, location),
            new Action.Parameter(Action.PARAMETER_NAME, downloadFileName),
            new Action.Parameter(Action.PARAMETER_MIME_TYPE, mimeType)
          },
          app.getTexts().getClickToDownloadText() + " " + downloadFileName,
          true
        );
      response.sendRedirect(
         request.getContextPath() + "/" + downloadAction.getEncodedHRef(requestId)
      );
%>
      <div style="background-color:#FFFFFF;border:1px solid #EBEBEB;padding:10px;margin-top:15px;">
        <a href="<%= request.getContextPath() + "/" + downloadAction.getEncodedHRef(requestId) %>"><%= app.getTexts().getClickToDownloadText() %> <b><%= downloadFileName %></b></a>
      </div>
<%
    }
    else {
      // Go back to previous view
      Action nextAction = new ObjectReference(
           (RefObject_1_0)pm.getObjectById(new Path(objectXri)),
           app
      ).getSelectObjectAction();
      response.sendRedirect(
        request.getContextPath() + "/" + nextAction.getEncodedHRef()
      );
    }
%>
              </form>
            </div> <!-- content -->
          </div> <!-- content-wrap -->
         <div> <!-- wrap -->
      </div> <!-- container -->
      </body>
      </html>
<%
  }
  catch(Exception e) {
    new ServiceException(e).log();
      // Go back to previous view
    Action nextAction = new ObjectReference(
         (RefObject_1_0)pm.getObjectById(new Path(objectXri)),
         app
    ).getSelectObjectAction();
    response.sendRedirect(
      request.getContextPath() + "/" + nextAction.getEncodedHRef()
    );
  } finally {
	  if(pm != null) {
		  pm.close();
	  }
  }
%>
