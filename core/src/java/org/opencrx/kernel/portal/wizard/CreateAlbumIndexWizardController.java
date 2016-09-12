/*
 * ====================================================================
 * Project:     openCRX/Core, http://www.opencrx.org/
 * Description: CreateAlbumIndexWizardController
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
 * CreateAlbumIndexWizardController
 *
 */
public class CreateAlbumIndexWizardController extends AbstractWizardController {

	/**
	 * Constructor.
	 * 
	 */
	public CreateAlbumIndexWizardController(
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
					indexDocument.getFolder().add(folder);					
					indexDocument.getOwningGroup().addAll(folder.getOwningGroup());					
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
					"  <link href=\"./css/blueimp-gallery.min.css\" rel=\"stylesheet\">\n" + 
					"  <style>\n" + 
					"    .thumbnail {\n" + 
					"	   margin-bottom: 0px;\n" + 
					"    }\n" + 
					"    .thumb {\n" + 
					"	   margin-bottom: 20px;\n" + 
					"    }\n" + 
					"  </style>\n" + 
					"</head>\n" + 
					"<body>\n" +
					"  <div class=\"container\">\n" + 
					"    <h1>Album for " + HtmlEncoder.encode(this.getQualifiedFolderName(folder, true), false) + "</h1>\n";
				if(folder.getDescription() != null) {
					albumIndex +=
						"    <blockquote>\n" +
						"      <p>" + HtmlEncoder.encode(folder.getDescription(), false) + "</p>\n" + 
						"    </blockquote>\n";
				}
				albumIndex += "    <div id=\"links\">\n";
				folderEntryQuery = (DocumentFolderEntryQuery)pm.newQuery(DocumentFolderEntry.class);
				folderEntryQuery.forAllDisabled().isFalse();
				folderEntryQuery.orderByName().ascending();
				albumIndex += "      <div class=\"container\"\n>";
				int index = 0;
				for(DocumentFolderEntry entry: folder.getFolderEntry(folderEntryQuery)) {
					if(!entry.getName().equals("index.html") && !".passwd".equals(entry.getName())) {
						if(entry.getDocument() instanceof Document) {
							Document document = (Document)entry.getDocument();							
							if(document.getContentType() != null && document.getContentType().startsWith("image/")) {
								String documentTitle = document.getTitle();
								String documentAbstract = document.getDocumentAbstract();
								try {
									String url = "./" + entry.getName();
									albumIndex +=
										"        <div class=\"col-sm-3 col-xs-6 thumb pull-right\">\n" + 
										"          <a href=\"" + url + "\" class=\"thumbnail\" data-gallery=\"\" title=\"" + HtmlEncoder.encode(documentTitle, false) + "\"><img class=\"img-responsive\" src=\"" + url + "\"></a><small><b>" + HtmlEncoder.encode(documentTitle, false) + "</b><p><i>" + HtmlEncoder.encode(entry.getName() + " / " + author + " " + entry.getModifiedAt(), false) + "</i><p>" + HtmlEncoder.encode(documentAbstract == null ? "" : documentAbstract, false) + "</small>\n" + 
										"        </div>\n";
									index++;
								} catch(Exception ignore) {}
								if(index % 4 == 0) {
									albumIndex += "      </div>\n";
									albumIndex += "      <div>\n";
								}
							} else {
								this.message = "Albums can be created for image folders only";
								return;
							}
						}
					}
				}
				albumIndex += "</div>";
				albumIndex +=
					"    </div>\n" + 
					"    <br>\n" + 
					"  </div>\n" + 
					"  <hr>\n" + 
					"  <footer>\n" + 
					"    <div class=\"row\">\n" + 
					"      <div class=\"col-lg-12\">\n" + 
					"         <p>Copyright &copy; " + new GregorianCalendar().get(Calendar.YEAR) + " " + HtmlEncoder.encode(author, false) + "</p>\n" + 
					"      </div>\n" +
					"    </div>\n" + 
					"  </footer>\n" + 
					"  <div class=\"blueimp-gallery\" id=\"blueimp-gallery\" style=\"display:none;\">\n" + 
					"    <!-- The container for the modal slides -->\n" + 
					"    <div class=\"slides\" style=\"width: 163100px;\"></div>\n" + 
					"    <!-- Controls for the borderless lightbox -->\n" + 
					"    <h3 class=\"title\"></h3>\n" + 
					"    <a class=\"prev\">‹</a> <a class=\"next\">›</a> <a class=\"close\">×</a> <a class=\"play-pause\"></a>\n" + 
					"    <ol class=\"indicator\"></ol>\n" + 
					"      <div class=\"modal fade\">\n" + 
					"        <div class=\"modal-dialog\">\n" + 
					"          <div class=\"modal-content\">\n" + 
					"            <div class=\"modal-header\">\n" + 
					"              <button aria-hidden=\"true\" class=\"close\" type=\"button\">×</button>\n" + 
					"              <h4 class=\"modal-title\"></h4>\n" +
					"            </div>\n" +
					"            <div class=\"modal-body next\"></div>\n" + 
					"            <div class=\"modal-footer\">\n" + 
					"              <button class=\"btn btn-default pull-left prev\" type=\"button\">\n" + 
					"                <i class=\"glyphicon glyphicon-chevron-left\"></i> Previous\n" + 
					"              </button>\n" + 
					"              <button class=\"btn btn-primary next\" type=\"button\">\n" + 
					"                Next <i class=\"glyphicon glyphicon-chevron-right\"></i>\n" + 
					"              </button>\n" + 
					"            </div>\n" + 
					"          </div>\n" + 
					"        </div>\n" + 
					"      </div>\n" + 
					"    </div>\n" + 
					"    <script src=\"./js/jquery.min.js\"></script>\n" + 
					"    <script src=\"./js/bootstrap.min.js\"></script>\n" + 
					"    <script src=\"./js/jquery.blueimp-gallery.min.js\"></script>\n" + 
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
