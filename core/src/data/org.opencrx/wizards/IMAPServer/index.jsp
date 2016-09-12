<%@ page contentType= "text/html;charset=UTF-8" language="java" pageEncoding= "UTF-8" %>
<%  request.setCharacterEncoding("UTF-8"); %>
<!DOCTYPE HTML PUBLIC "-//W3C//DTD HTML 4.01 Transitional//EN" "http://www.w3.org/TR/html4/loose.dtd">

<html>

<head>
  <title>openCRX IMAP Server</title>
  <meta http-equiv="Content-Type" content="text/html; charset=utf-8">
</head>

<%
	response.sendRedirect(request.getContextPath().replace("-core-", "-imap-") + "/IMAPServlet/");
%>

</html>
