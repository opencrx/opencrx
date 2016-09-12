<%@  page contentType="text/html;charset=UTF-8" language="java" pageEncoding="UTF-8" %><%
/*
 * ====================================================================
 * Project:     openCRX/Core, http://www.opencrx.org/
 * Description: Documents Finder
 * Owner:       CRIXP Corp., Switzerland, http://www.crixp.com
 * ====================================================================
 *
 * This software is published under the BSD license
 * as listed below.
 *
 * Copyright (c) 2008-2014, CRIXP Corp., Switzerland
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
java.math.*
" %>
<%
	String id = request.getParameter("id");
	String lang = request.getParameter("lang");
	if(lang != null) {
		if(!"zh_CN".equals(lang) && !"zh_TW".equals(lang) && !"pt_BR".equals(lang)) {
			lang = lang.substring(0, 2);
		}
	}
%>
<!DOCTYPE html>
<html>
<head>
	<meta charset="utf-8">
	<title><%= id %></title>
	<script src="jquery/jquery.min.js" type="text/javascript" charset="utf-8"></script>
	<script src="jquery/jquery-ui-1.10.1.custom.min.js" type="text/javascript" charset="utf-8"></script>

	<link rel="shortcut icon" href="img/favicon.ico" />
	<link rel="stylesheet" href="jquery/ui-themes/smoothness/jquery-ui-1.10.1.custom.css" />
	<link rel="stylesheet" href="css/common.css" type="text/css" />
	<link rel="stylesheet" href="css/dialog.css" type="text/css" />
	<link rel="stylesheet" href="css/toolbar.css" type="text/css" />
	<link rel="stylesheet" href="css/navbar.css" type="text/css" />
	<link rel="stylesheet" href="css/statusbar.css" type="text/css" />
	<link rel="stylesheet" href="css/contextmenu.css" type="text/css" />
	<link rel="stylesheet" href="css/cwd.css" type="text/css" />
	<link rel="stylesheet" href="css/quicklook.css" type="text/css" />
	<link rel="stylesheet" href="css/commands.css" type="text/css" />
	<link rel="stylesheet" href="css/fonts.css" type="text/css" />
	<link rel="stylesheet" href="css/theme.css" type="text/css" />

	<!-- elfinder core -->
	<script src="js/elFinder.js"></script>
	<script src="js/elFinder.version.js"></script>
	<script src="js/jquery.elfinder.js"></script>
	<script src="js/elFinder.resources.js"></script>
	<script src="js/elFinder.options.js"></script>
	<script src="js/elFinder.history.js"></script>
	<script src="js/elFinder.command.js"></script>

	<!-- elfinder ui -->
	<script src="js/ui/overlay.js"></script>
	<script src="js/ui/workzone.js"></script>
	<script src="js/ui/navbar.js"></script>
	<script src="js/ui/dialog.js"></script>
	<script src="js/ui/tree.js"></script>
	<script src="js/ui/cwd.js"></script>
	<script src="js/ui/toolbar.js"></script>
	<script src="js/ui/button.js"></script>
	<script src="js/ui/uploadButton.js"></script>
	<script src="js/ui/viewbutton.js"></script>
	<script src="js/ui/searchbutton.js"></script>
	<script src="js/ui/sortbutton.js"></script>
	<script src="js/ui/panel.js"></script>
	<script src="js/ui/contextmenu.js"></script>
	<script src="js/ui/path.js"></script>
	<script src="js/ui/stat.js"></script>
	<script src="js/ui/places.js"></script>

	<!-- elfinder commands -->
	<script src="js/commands/back.js"></script>
	<script src="js/commands/forward.js"></script>
	<script src="js/commands/reload.js"></script>
	<script src="js/commands/up.js"></script>
	<script src="js/commands/home.js"></script>
	<script src="js/commands/copy.js"></script>
	<script src="js/commands/cut.js"></script>
	<script src="js/commands/paste.js"></script>
	<script src="js/commands/open.js"></script>
	<script src="js/commands/opencontainingfolder.js"></script>
	<script src="js/commands/rm.js"></script>
	<script src="js/commands/info.js"></script>
	<script src="js/commands/duplicate.js"></script>
	<script src="js/commands/rename.js"></script>
	<script src="js/commands/help.js"></script>
	<script src="js/commands/getfile.js"></script>
	<script src="js/commands/mkdir.js"></script>
	<script src="js/commands/mkfile.js"></script>
	<script src="js/commands/upload.js"></script>
	<script src="js/commands/download.js"></script>
	<script src="js/commands/edit.js"></script>
	<script src="js/commands/quicklook.js"></script>
	<script src="js/commands/quicklook.plugins.js"></script>
	<script src="js/commands/extract.js"></script>
	<script src="js/commands/archive.js"></script>
	<script src="js/commands/search.js"></script>
	<script src="js/commands/view.js"></script>
	<script src="js/commands/resize.js"></script>
	<script src="js/commands/sort.js"></script>	
	<script src="js/commands/netmount.js"></script>	

	<!-- elfinder languages -->
	<script src="js/i18n/elfinder.ar.js"></script>
	<script src="js/i18n/elfinder.bg.js"></script>
	<script src="js/i18n/elfinder.ca.js"></script>
	<script src="js/i18n/elfinder.cs.js"></script>
	<script src="js/i18n/elfinder.de.js"></script>
	<script src="js/i18n/elfinder.el.js"></script>
	<script src="js/i18n/elfinder.en.js"></script>
	<script src="js/i18n/elfinder.es.js"></script>
	<script src="js/i18n/elfinder.fa.js"></script>
	<script src="js/i18n/elfinder.fr.js"></script>
	<script src="js/i18n/elfinder.hu.js"></script>
	<script src="js/i18n/elfinder.it.js"></script>
	<script src="js/i18n/elfinder.jp.js"></script>
	<script src="js/i18n/elfinder.ko.js"></script>
	<script src="js/i18n/elfinder.nl.js"></script>
	<script src="js/i18n/elfinder.no.js"></script>
	<script src="js/i18n/elfinder.pl.js"></script>
	<script src="js/i18n/elfinder.pt_BR.js"></script>
	<script src="js/i18n/elfinder.ru.js"></script>
	<script src="js/i18n/elfinder.sl.js"></script>
	<script src="js/i18n/elfinder.sv.js"></script>
	<script src="js/i18n/elfinder.tr.js"></script>
	<script src="js/i18n/elfinder.zh_CN.js"></script>
	<script src="js/i18n/elfinder.zh_TW.js"></script>
	<script src="js/i18n/elfinder.vi.js"></script>

	<!-- elfinder dialog -->
	<script src="js/jquery.dialogelfinder.js"></script>

	<!-- elfinder 1.x connector API support -->
	<script src="js/proxy/elFinderSupportVer1.js"></script>

	<!-- elfinder custom extenstions -->
	<script src="extensions/jplayer/elfinder.quicklook.jplayer.js"></script>

	<style type="text/css">
		body { 
			font-family:arial, verdana, sans-serif;
		}
	</style>
	<script>
		$(document).ready(function() {
			$('#finder').elfinder({
				requestType : 'POST',
				url : 'connector<%= id.startsWith("/") ? id : "/" + id %>',
				lang : '<%= lang %>',
				rememberLastDir : true,
				ui : ['tree', 'toolbar', 'path', 'stat'],
				commands : [
				  'open', 'opencontainingfolder', 'reload', 'home', 'up', 'back', 'forward', 'getfile', 'quicklook', 
				  'download', 'rm', 'rename', 'mkdir', 'upload', 
				  'search', 'info', 'view', 'help', 'sort'],
				toolbar : [
					['back', 'forward'],
					['reload'],
					['home', 'up'],
					['mkdir', 'mkfile', 'upload'],
					['open', 'download'],
					['info', 'quicklook', 'rm', 'rename'],
					['edit'],
					['search'],
					['view', 'sort'],
					['help']
				],
				contextmenu : {
					// navbarfolder menu
					navbar : ['open', '|', 'copy', 'cut', 'paste', 'duplicate', '|', 'rm', '|', 'info'],
					// current directory menu
					cwd    : ['reload', 'back', '|', 'upload', 'mkdir', 'mkfile', 'paste', '|', 'sort', '|', 'info'],
					// current directory file menu
					files  : ['getfile', '|', 'open', 'opencontainingfolder', 'quicklook', '|', 'download', '|', 'copy', 'cut', 'paste', 'duplicate', '|', 'rm', '|', 'edit', 'rename', 'resize', '|', 'archive', 'extract', '|', 'info']
				}
			});
		});
	</script>
</head>
<body>
	<div style="height:98vh" id="finder">Finder<span>here</span></div>
	<br clear="all"/>
</body>
</html>
