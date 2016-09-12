/*
 * ====================================================================
 * Project:     openCRX/Core, http://www.opencrx.org/
 * Description: CreateListingIndexWizardController
 * Owner:       CRIXP AG, Switzerland, http://www.crixp.com
 * ====================================================================
 *
 * This software is published under the BSD license
 * as listed below.
 * 
 * Copyright (c) 2014, CRIXP Corp., Switzerland
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
package org.opencrx.kernel.portal.wizard;

import java.util.Calendar;
import java.util.GregorianCalendar;
import java.util.List;

import javax.jdo.PersistenceManager;

import org.opencrx.kernel.backend.Documents;
import org.opencrx.kernel.document1.cci2.DocumentFolderEntryQuery;
import org.opencrx.kernel.document1.jmi1.Document;
import org.opencrx.kernel.document1.jmi1.DocumentFolder;
import org.opencrx.kernel.document1.jmi1.DocumentFolderEntry;
import org.opencrx.kernel.utils.Utils;
import org.openmdx.base.exception.ServiceException;
import org.openmdx.base.text.conversion.HtmlEncoder;
import org.openmdx.portal.servlet.AbstractWizardController;
import org.openmdx.portal.servlet.ApplicationContext;
import org.openmdx.portal.servlet.ObjectReference;
import org.w3c.cci2.BinaryLargeObjects;

/**
 * CreateListingIndexWizardController
 *
 */
public class CreateListingIndexWizardController extends AbstractWizardController {

	/**
	 * Constructor.
	 * 
	 */
	public CreateListingIndexWizardController(
	) {
		super();
	}
	
	/**
	 * Get qualified folder name.
	 * 
	 * @param folder
	 * @return
	 */
	protected String getQualifiedFolderName(
		DocumentFolder folder,
		boolean includeRoot
	) {
		boolean isRootFolder = folder.getParent() == null || "Spaces".equals(folder.getParent().getName());
		return isRootFolder
			? includeRoot ? folder.getName() : ""
			: this.getQualifiedFolderName(folder.getParent(), includeRoot) + "/" + folder.getName();
	}

	/**
	 * Cancel action.
	 * 
	 */
	public void doCancel(
	) {
		this.setExitAction(
			new ObjectReference(this.getObject(), this.getApp()).getSelectObjectAction()
		);
	}
	
	/**
	 * Refresh action.
	 * 
	 */
	public void doRefresh(
	) throws ServiceException {
		PersistenceManager pm = this.getPm();
		ApplicationContext app = this.getApp();
		if(this.getObject() instanceof DocumentFolder) {
			org.opencrx.kernel.document1.jmi1.Segment documentSegment = Documents.getInstance().getDocumentSegment(pm, this.getProviderName(), this.getSegmentName());
			DocumentFolder folder = (DocumentFolder)this.getObject();
			String author = app.getUserHomeIdentityAsPath().getBase();
			// Find index.html
			Document indexDocument = null;
			DocumentFolderEntryQuery folderEntryQuery = (DocumentFolderEntryQuery)pm.newQuery(DocumentFolderEntry.class);
			folderEntryQuery.name().equalTo("index.html");
			folderEntryQuery.forAllDisabled().isFalse();
			List<DocumentFolderEntry> entries = folder.getFolderEntry(folderEntryQuery);
			if(!entries.isEmpty()) {
				DocumentFolderEntry entry = entries.iterator().next();
				if(entry.getDocument() instanceof Document) {
					indexDocument = (Document)entry.getDocument();
				}
			}
			if(indexDocument == null) {
				try {
					pm.currentTransaction().begin();
					indexDocument = pm.newInstance(Document.class);
					indexDocument.setName("index.html");
					indexDocument.setContentType("text/html");
					indexDocument.setAuthor(author);
					indexDocument.getOwningGroup().addAll(folder.getOwningGroup());
					indexDocument.getFolder().add(folder);
					documentSegment.addDocument(
						Utils.getUidAsString(),
						indexDocument
					);
					pm.currentTransaction().commit();
				} catch(Exception e) {
					try {
						pm.currentTransaction().rollback();
					} catch(Exception ignore) {}
					this.message = "Unable to create index document. Reason is " + e.getMessage();
				}
			}
			if(indexDocument != null) {
				String albumIndex = 
					"<!DOCTYPE html>\n" + 
					"<html lang=\"en\">\n" + 
					"<head>\n" + 
					"  <meta charset=\"utf-8\">\n" + 
					"  <title>" + HtmlEncoder.encode(this.getQualifiedFolderName(folder, true), false) + "</title>\n" + 
					"  <meta content=\"width=device-width, initial-scale=1.0\" name=\"viewport\">\n" + 
					"  <link href=\"./css/bootstrap.min.css\" rel=\"stylesheet\">\n" + 
					"</head>\n" + 
					"<body>\n" + 
					"  <div class=\"container\">\n" + 
					"    <h1>Listing of " + HtmlEncoder.encode(this.getQualifiedFolderName(folder, true), false) + "</h1>\n";
				if(folder.getDescription() != null) {
					albumIndex +=
						"    <blockquote>\n" + 
						"      <p>" + HtmlEncoder.encode(folder.getDescription(), false) + "</p>\n" + 
						"    </blockquote>\n";
				}
				folderEntryQuery = (DocumentFolderEntryQuery)pm.newQuery(DocumentFolderEntry.class);
				folderEntryQuery.forAllDisabled().isFalse();
				folderEntryQuery.orderByName().ascending();
				albumIndex += "    <table class=\"table table-hover table-striped table-condensed\">";
				albumIndex += "      <tr>\n";
				albumIndex += "        <th>Name</th>\n";
				albumIndex += "        <th>Last modified</th>\n";
				albumIndex += "        <th>Description</th>\n";
				albumIndex += "      </tr>";
				for(DocumentFolderEntry entry: folder.getFolderEntry(folderEntryQuery)) {
					if(!entry.getName().equals("index.html") && !".passwd".equals(entry.getName())) {
						try {
							String url = "./" + entry.getName();
							albumIndex += "      <tr>\n";
							albumIndex += "        <td><a href=\"" + url + "\">" + HtmlEncoder.encode(entry.getName(), false) + "</a></td>\n";
							albumIndex += "        <td>" + entry.getModifiedAt() + "</td>\n";
							albumIndex += "        <td>" + HtmlEncoder.encode(entry.getDescription() == null ? "" : entry.getDescription(), false) + "</td>\n";
							albumIndex += "      <tr>\n";
						} catch(Exception ignore) {}
					}
				}
				albumIndex += "    </table>";
				albumIndex += "  </div>";
				albumIndex +=
					"  <hr>\n" + 
					"  <footer>\n" + 
					"    <div class=\"row\">\n" + 
					"      <div class=\"col-lg-12\">\n" + 
					"         <p>Copyright &copy; " + new GregorianCalendar().get(Calendar.YEAR) + " " + HtmlEncoder.encode(author, false) + "</p>\n" + 
					"      </div>\n" +
					"    </div>\n" + 
					"  </footer>\n" + 
					"    <script src=\"./js/jquery.min.js\"></script>\n" + 
					"    <script src=\"./js/bootstrap.min.js\"></script>\n" + 
					"  </body>\n" + 
					"</html>";
				try {
					pm.currentTransaction().begin();
					Documents.getInstance().addRevision(
						indexDocument, 
						"index.html", 
						"text/html", 
						author, 
						BinaryLargeObjects.valueOf(albumIndex.getBytes("UTF-8"))
					);
					pm.currentTransaction().commit();
					this.message = "Successfully created/updated index";
				} catch(Exception e) {
					try {
						pm.currentTransaction().rollback();
					} catch(Exception ignore) {}
					this.message = "Unable to create/update index. Reason is " + e.getMessage();
				}
			}
		}
	}

	/**
	 * @return the errors
	 */
	public String getMessage() {
		return message;
	}

	private String message = null;

}
