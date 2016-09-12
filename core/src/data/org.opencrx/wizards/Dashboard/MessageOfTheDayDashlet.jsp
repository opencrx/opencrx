<%@page contentType="text/html;charset=UTF-8" language="java" pageEncoding="UTF-8"%><%
/*
 * ====================================================================
 * Project:     openCRX/Core, http://www.opencrx.org/
 * Description: MessageOfTheDayDashlet
 * Owner:       CRIXP Corp., Switzerland, http://www.crixp.com
 * ====================================================================
 *
 * This software is published under the BSD license
 * as listed below.
 *
 * Copyright (c) 2009-2013, CRIXP Corp., Switzerland
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
%>
<%@ page session="true" import="
java.util.*,
java.util.zip.*,
java.io.*,
java.text.*,
java.math.*,
java.net.*,
org.openmdx.base.accessor.jmi.cci.*,
org.openmdx.base.naming.*,
org.openmdx.base.exception.*,
org.openmdx.portal.servlet.*,
org.opencrx.kernel.backend.*,
org.opencrx.kernel.generic.*,
org.openmdx.kernel.log.*
" %>

<%!
	final static String DOCUMENT_PREFIX = "document:";

	public org.opencrx.kernel.document1.jmi1.Document findDocument(
		String documentName,
		org.opencrx.kernel.document1.jmi1.Segment segment
	) throws ServiceException {
		return Documents.getInstance().findDocument(
			documentName,
			segment
		);
	}

	public String documentContentToString(
		org.opencrx.kernel.document1.jmi1.Document document,
		List<String> documentNames,
		org.opencrx.kernel.document1.jmi1.Segment documentSegment
	) {
		String content = "";
		if (document != null) {
			try {
				org.opencrx.kernel.document1.jmi1.MediaContent headRevision =
					(org.opencrx.kernel.document1.jmi1.MediaContent)document.getHeadRevision();
				ByteArrayOutputStream bos = new ByteArrayOutputStream();
				org.w3c.cci2.BinaryLargeObjects.streamCopy(
					headRevision.getContent().getContent(),
					0L,
					bos
				);
				bos.close();
				String[] pieces = (new String(bos.toByteArray(), "UTF-8")).split(DOCUMENT_PREFIX);
				for(int idx = 0; pieces != null && idx < pieces.length;) {
					content += pieces[idx++];
					if (idx < pieces.length) {
						// test for valid document reference (by document name)
						int endPos = pieces[idx].indexOf("\\");
						String documentName = (pieces[idx].substring(0, endPos)).trim();
						if (!documentNames.contains(documentName)) { /* avoid endless recursion! */
							documentNames.add(documentName);
							org.opencrx.kernel.document1.jmi1.Document docToInclude = findDocument (
								documentName,
								documentSegment
							);
							content += documentContentToString(docToInclude, documentNames, documentSegment);
						} else {
							content += "(WARNING: DocumentRecursion!)";
						}
						pieces[idx] = pieces[idx].substring(endPos+1);
					}
				}
			} catch (Exception e) {
				new ServiceException(e).log();
			}
		}
		return content;
	}

%><%
	final String MESSAGE_OF_THE_DAY_DOCUMENT_NAME = "Message of the day.html";
	ApplicationContext app = (ApplicationContext)session.getValue(WebKeys.APPLICATION_KEY);
	ViewsCache viewsCache = (ViewsCache)session.getValue(WebKeys.VIEW_CACHE_KEY_SHOW);
	String parameters = request.getParameter(WebKeys.REQUEST_PARAMETER);
	if(app != null && parameters != null) {
		String xri = Action.getParameter(parameters, Action.PARAMETER_OBJECTXRI);
		String requestId = request.getParameter(Action.PARAMETER_REQUEST_ID);
		String dashletId = Action.getParameter(parameters, Action.PARAMETER_ID);
%>
		<div>
<%				
			if(xri != null && requestId != null && dashletId != null && viewsCache.getView(requestId) != null) {
				javax.jdo.PersistenceManager pm = app.getNewPmData();
				RefObject_1_0 obj = (RefObject_1_0)pm.getObjectById(new Path(xri));
				String providerName = obj.refGetPath().get(2);
				String segmentName = obj.refGetPath().get(4);
				org.opencrx.kernel.document1.jmi1.Segment documentSegment = Documents.getInstance().getDocumentSegment(pm, providerName, segmentName);
				List<String> documentNames = new ArrayList<String>();
				documentNames.add(MESSAGE_OF_THE_DAY_DOCUMENT_NAME);
				String messageOfTheDay = "";
				try {
					org.opencrx.kernel.document1.jmi1.Document messageOfTheDayDoc = findDocument (
						MESSAGE_OF_THE_DAY_DOCUMENT_NAME,
						documentSegment
					);
					messageOfTheDay += "<pre>" + messageOfTheDayDoc.getModifiedAt() + "</pre>";
					messageOfTheDay += documentContentToString(
						messageOfTheDayDoc,
						documentNames,
						documentSegment
				 	);
					messageOfTheDay += "<br />";
					if(messageOfTheDay.isEmpty()) {
						messageOfTheDay += "<pre> no message as of " + new Date() + "</pre>";
					}
				} catch (Exception e) {
					new ServiceException(e).log();
%>
					<p>
				    <i>Dashlet Exception - see log file for details</i>
			    </p>
<%
				}
				pm.close();
%>
				<%= messageOfTheDay %>
<%
			} else {
%>
				<p>
			    <i>Dashlet invoked with missing or invalid parameters:</i>
			    <ul>
				    <li><b>RequestId:</b> <%= requestId %></li>
				    <li><b>XRI:</b> <%= xri %></li>
				    <li><b>Dashlet-Id:</b> <%= dashletId %></li>
					</ul>
				</p>
<%
			}
%>		     
		</div>
<%			
  	}
%>
