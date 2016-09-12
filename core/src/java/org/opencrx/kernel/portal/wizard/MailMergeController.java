/*
 * ====================================================================
 * Project:     openCRX/Core, http://www.opencrx.org/
 * Description: MailMergeController
 * Owner:       CRIXP AG, Switzerland, http://www.crixp.com
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
package org.opencrx.kernel.portal.wizard;

import java.io.ByteArrayInputStream;
import java.io.ByteArrayOutputStream;
import java.io.FileOutputStream;
import java.io.InputStream;
import java.io.InputStreamReader;
import java.net.URLEncoder;
import java.text.DateFormat;
import java.text.DecimalFormat;
import java.text.NumberFormat;
import java.text.SimpleDateFormat;
import java.util.ArrayList;
import java.util.Iterator;
import java.util.List;
import java.util.TimeZone;
import java.util.zip.ZipEntry;
import java.util.zip.ZipOutputStream;

import javax.jdo.PersistenceManager;

import org.opencrx.kernel.account1.jmi1.Account;
import org.opencrx.kernel.account1.jmi1.AccountAddress;
import org.opencrx.kernel.account1.jmi1.AccountFilterGlobal;
import org.opencrx.kernel.account1.jmi1.AddressFilterGlobal;
import org.opencrx.kernel.account1.jmi1.Contact;
import org.opencrx.kernel.account1.jmi1.Group;
import org.opencrx.kernel.account1.jmi1.LegalEntity;
import org.opencrx.kernel.account1.jmi1.Member;
import org.opencrx.kernel.account1.jmi1.PostalAddress;
import org.opencrx.kernel.account1.jmi1.UnspecifiedAccount;
import org.opencrx.kernel.activity1.jmi1.AbstractMailingRecipient;
import org.opencrx.kernel.activity1.jmi1.AddressGroup;
import org.opencrx.kernel.activity1.jmi1.AddressGroupMember;
import org.opencrx.kernel.activity1.jmi1.Mailing;
import org.opencrx.kernel.activity1.jmi1.MailingRecipient;
import org.opencrx.kernel.activity1.jmi1.MailingRecipientGroup;
import org.opencrx.kernel.backend.Base;
import org.opencrx.kernel.backend.Documents;
import org.opencrx.kernel.document1.cci2.DocumentFolderEntryQuery;
import org.opencrx.kernel.document1.cci2.DocumentFolderQuery;
import org.opencrx.kernel.document1.jmi1.Document;
import org.opencrx.kernel.document1.jmi1.DocumentFolder;
import org.opencrx.kernel.document1.jmi1.DocumentFolderEntry;
import org.opencrx.kernel.document1.jmi1.MediaContent;
import org.opencrx.kernel.document1.jmi1.MediaReference;
import org.openmdx.base.exception.ServiceException;
import org.openmdx.base.naming.Path;
import org.openmdx.portal.servlet.AbstractWizardController;
import org.openmdx.portal.servlet.Action;
import org.openmdx.portal.servlet.ApplicationContext;
import org.openmdx.portal.servlet.Codes;
import org.openmdx.portal.servlet.ObjectReference;
import org.w3c.cci2.BinaryLargeObjects;

/**
 * MailMergeController
 *
 */
public class MailMergeController extends AbstractWizardController {

	/**
	 * Constructor.
	 * 
	 */
	public MailMergeController(
	) {
		super();
	}
	
	/**
	 * MailMergeTemplate
	 *
	 */
	public static class MailMergeTemplate {
		
		/**
		 * @return the mediaContentIdentity
		 */
		public Path getMediaContentIdentity() {
			return mediaContentIdentity;
		}
		/**
		 * @param mediaContentIdentity the mediaContentIdentity to set
		 */
		public void setMediaContentIdentity(Path mediaContentIdentity) {
			this.mediaContentIdentity = mediaContentIdentity;
		}
		/**
		 * @return the entryName
		 */
		public String getEntryName() {
			return entryName;
		}
		/**
		 * @param entryName the entryName to set
		 */
		public void setEntryName(String entryName) {
			this.entryName = entryName;
		}
		/**
		 * @return the contentName
		 */
		public String getContentName() {
			return contentName;
		}
		/**
		 * @param contentName the contentName to set
		 */
		public void setContentName(String contentName) {
			this.contentName = contentName;
		}
		private String entryName;
		private String contentName;
		private Path mediaContentIdentity;
	}

	/**
	 * Find document templates
	 * 
	 * @param documentFolderName
	 * @param documentSegment
	 * @return
	 */
	public List<MailMergeTemplate> getMailMergeTemplates(
	) throws ServiceException {
		PersistenceManager pm = this.getPm();
		List<MailMergeTemplate> documentTemplates = new ArrayList<MailMergeTemplate>();
		org.opencrx.kernel.document1.jmi1.Segment documentSegment = Documents.getInstance().getDocumentSegment(pm, this.getProviderName(), this.getSegmentName());
		DocumentFolderQuery query = (DocumentFolderQuery)pm.newQuery(DocumentFolder.class);
		query.name().equalTo(MAILMERGE_TEMPLATE_FOLDER_NAME);
		List<DocumentFolder> documentFolders = documentSegment.getFolder(query);
		if(!documentFolders.isEmpty()) {
			DocumentFolder documentFolder = documentFolders.iterator().next();
			DocumentFolderEntryQuery documentFolderEntryFilter = (DocumentFolderEntryQuery)pm.newQuery(DocumentFolderEntry.class);
			documentFolderEntryFilter.forAllDisabled().isFalse();
			documentFolderEntryFilter.orderByName().ascending();			
	        for(org.opencrx.kernel.document1.jmi1.DocumentFolderEntry entry: documentFolder.getFolderEntry(documentFolderEntryFilter)) {
				if(entry.getDocument() instanceof Document) {
					Document document = (Document)entry.getDocument();
					try {
					    // get content of head revision
					    if (document.getHeadRevision() != null) {
							MediaContent mediaContent = null;
							if(document.getHeadRevision() instanceof MediaReference) {
								MediaReference mediaReference = (MediaReference)document.getHeadRevision();
								if (mediaReference.getMedia() != null) {
								  mediaContent = (MediaContent)((org.opencrx.kernel.document1.jmi1.Media)mediaReference.getMedia());
								}
							} else {
						        if (document.getHeadRevision() instanceof MediaContent) {
						          mediaContent = (MediaContent)document.getHeadRevision();
						        }
							}
							if (
								(mediaContent != null) &&
								(mediaContent.getContentMimeType() != null) &&
								(MailMergeController.TEMPLATE_MIMETYPE1.equals(mediaContent.getContentMimeType()) || MailMergeController.TEMPLATE_MIMETYPE2.equals(mediaContent.getContentMimeType()))
							) {
								MailMergeTemplate documentTemplate = new MailMergeTemplate();
								documentTemplate.setEntryName(entry.getName());
								documentTemplate.setContentName(mediaContent.getContentName());
								documentTemplate.setMediaContentIdentity(mediaContent.refGetPath());
								documentTemplates.add(documentTemplate);
							}
					    }
					} catch(Exception ignore) {}
				}
	        }
		}
		return documentTemplates;
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
	 * @param templateXri
	 * @param lastSelection
	 */
	public void doRefresh(
		@RequestParameter(name = "templateXri") String templateXri
	) {
		this.templateXri = templateXri;
	}

	/**
	 * OK action.
	 * 
	 * @param templateXri
	 * @param lastSelection
	 * @throws ServiceException
	 */
	public void doOK(
		@RequestParameter(name = "templateXri") String templateXri
	) throws ServiceException {
		PersistenceManager pm = this.getPm();
		ApplicationContext app = this.getApp();
		Codes codes = this.getCodes();
		this.doRefresh(
			templateXri
		);
		if(templateXri != null) {
			try {
				TimeZone tz = TimeZone.getTimeZone(app.getCurrentTimeZone());
				DateFormat timestamp = new SimpleDateFormat("yyyy-MM-dd'T'HH:mm:ss.SSS'Z'");
				timestamp.setLenient(false);
				timestamp.setTimeZone(tz);
				byte[] template = null;
				{
					MediaContent mediaContent = (MediaContent)pm.getObjectById(new Path(templateXri));
					InputStream content = mediaContent.getContent().getContent();
					ByteArrayOutputStream bytes = new ByteArrayOutputStream();
					BinaryLargeObjects.streamCopy(content, 0L, bytes);
					bytes.close();
					template = bytes.toByteArray();
				}
				Account account = null;
				List<Path> postalAddressesXri = new ArrayList<Path>();
				boolean isAccount = false;
				boolean isAddressFilterGlobal = false;
				boolean isAddressGroupMember = false;
				boolean isAccountFilterGlobal = false;
				boolean isMailingActivityRecipient = false;
				Iterator<?> i = null;
				if (this.getObject() instanceof Group) {
					i = ((Group)this.getObject()).getMember().iterator();
				} else if (this.getObject() instanceof AccountFilterGlobal) {
					i = ((AccountFilterGlobal)this.getObject()).getFilteredAccount().iterator();
					isAccountFilterGlobal = true;
				} else if (this.getObject() instanceof AddressFilterGlobal) {
					i = ((AddressFilterGlobal)this.getObject()).getFilteredAddress().iterator();
					isAddressFilterGlobal = true;
				} else if (this.getObject() instanceof AddressGroup) {
					i = ((AddressGroup)this.getObject()).getMember().iterator();
					isAddressGroupMember = true;
				} else if (this.getObject() instanceof Mailing) {
					i = ((Mailing)this.getObject()).getMailingRecipient().iterator();
					isMailingActivityRecipient = true;
				} else if (
					(this.getObject() instanceof Contact) ||
					(this.getObject() instanceof LegalEntity) ||
					(this.getObject() instanceof UnspecifiedAccount)
				) {
					isAccount = true;
					account = (Account)this.getObject();
				} else if (this.getObject() instanceof PostalAddress) {
					postalAddressesXri.add(this.getObject().refGetPath());
				}
				while (
					((i != null) && i.hasNext()) ||
					(isAccount && (account != null))
				) {
					if (isAddressFilterGlobal) {
						AccountAddress accountAddress = (AccountAddress)i.next();
						if (accountAddress instanceof PostalAddress) {
							postalAddressesXri.add(accountAddress.refGetPath());
						}
					} else {
						if (isAddressGroupMember) {
							AddressGroupMember addressGroupMember = (AddressGroupMember)i.next();
							if (addressGroupMember.getAddress() instanceof PostalAddress) {
								postalAddressesXri.add(addressGroupMember.getAddress().refGetPath());
							}
						} else {
							if (isMailingActivityRecipient) {
								AbstractMailingRecipient abstractMailingRecipient = (AbstractMailingRecipient)i.next();
								if (abstractMailingRecipient instanceof MailingRecipient) {
									MailingRecipient mailingRecipient = (MailingRecipient)abstractMailingRecipient;
									if (mailingRecipient.getParty() instanceof PostalAddress) {
										postalAddressesXri.add(mailingRecipient.getParty().refGetPath());
									}
								} else {
									if (abstractMailingRecipient instanceof MailingRecipientGroup) {
										MailingRecipientGroup mailingRecipientGroup = (MailingRecipientGroup)abstractMailingRecipient;
										if (mailingRecipientGroup.getParty() instanceof AddressGroup) {
											AddressGroup addressGroup = (AddressGroup)mailingRecipientGroup.getParty();
											for(AddressGroupMember addressGroupMember: addressGroup.<AddressGroupMember>getMember()) {
												if (addressGroupMember.getAddress() instanceof PostalAddress) {
													postalAddressesXri.add(addressGroupMember.getAddress().refGetPath());
												}
											}
										}
									}
								}
							} else {
								if (!isAccount) {
									if (isAccountFilterGlobal) {
										account = (Account)i.next();
									} else {
										Member member = (Member)i.next();
										// Skip disabled members
										if (Boolean.TRUE.equals(member.isDisabled())) {
											continue;
										} else {
											account = member.getAccount();
										}
										// Skip disabled accounts
										if((account != null) && Boolean.TRUE.equals(account.isDisabled())) {
											continue;
										}
									}
								}
								if(account != null) {
									PostalAddress mailingAddress = null;
									boolean searchingMainMailingAddress = true;
									for(
										Iterator<AccountAddress> addr = account.<AccountAddress>getAddress().iterator();
										addr.hasNext() && searchingMainMailingAddress;
									) {
										AccountAddress address = (AccountAddress)addr.next();
										if (!(address instanceof PostalAddress)) {continue;}
										mailingAddress = (PostalAddress)address;
										searchingMainMailingAddress = !mailingAddress.isMain();
									}
									if (mailingAddress != null) {
										postalAddressesXri.add(mailingAddress.refGetPath());
									}
								}
								account = null; /* ensure termination if isAccount==true !!! */
							}
						}
					}
				}
				// Generate document(s)
				org.opencrx.kernel.utils.rtf.RTFTemplate document = new org.opencrx.kernel.utils.rtf.RTFTemplate();
				document.readFrom(new InputStreamReader(new ByteArrayInputStream(template)), true);
				org.opencrx.kernel.utils.rtf.Bookmark bmTemplateRow = null;
				String[] templateRowLayout = null;
				try {
					bmTemplateRow = document.searchBookmark("TemplateRow");
					String layoutDefinition = bmTemplateRow.getRawContent();
					if(layoutDefinition != null && layoutDefinition.indexOf(" ") > 0) {
						layoutDefinition = layoutDefinition.substring(layoutDefinition.lastIndexOf(" ")).trim();
					}
					templateRowLayout = bmTemplateRow == null || layoutDefinition == null
						? null 
						: layoutDefinition.split(";");

				} catch(Exception e) {}
				int nRecordsPerRow = (templateRowLayout == null) || (templateRowLayout.length < 2) 
					? 1 
					: Integer.valueOf(templateRowLayout[1]).intValue();
				int counter = 0;
				int recordIndex = -1;
				String location = Base.getInstance().getUidAsString();
				FileOutputStream fileos = new FileOutputStream(app.getTempFileName(location, ""));		        
				ZipOutputStream zipos = new ZipOutputStream(fileos);
				for(Path postalAddressXri: postalAddressesXri) {
					counter += 1;
					PostalAddress mailingAddress = (PostalAddress)pm.getObjectById(postalAddressXri);
					if(bmTemplateRow != null) {
						if((recordIndex == -1) || (recordIndex == nRecordsPerRow)) {
							document.appendTableRow("TemplateRow");
							recordIndex = 0;
						}
					} else {
						document = new org.opencrx.kernel.utils.rtf.RTFTemplate();
						document.readFrom(new InputStreamReader(new ByteArrayInputStream(template)), true);
					}
					org.opencrx.kernel.utils.rtf.MultiTextParts mtpWarning = new org.opencrx.kernel.utils.rtf.MultiTextParts();
					mtpWarning.addText(org.opencrx.kernel.utils.rtf.TextPart.NEWLINE);
					// Map mailing address
					org.opencrx.kernel.utils.rtf.MultiTextParts mtpMailingAddress = new org.opencrx.kernel.utils.rtf.MultiTextParts();
					boolean needsNewLine = false;
					if (mailingAddress != null) {
						try {
							for(Iterator<String> m = mailingAddress.getPostalAddressLine().iterator(); m.hasNext();) {
								String lineToAdd = m.next();
								if(lineToAdd != null) {
									lineToAdd = lineToAdd.trim();
									if(!lineToAdd.isEmpty()) {
										if(needsNewLine) mtpMailingAddress.addText(org.opencrx.kernel.utils.rtf.TextPart.NEWLINE);
										mtpMailingAddress.addText(new org.opencrx.kernel.utils.rtf.TextPart(lineToAdd));
										needsNewLine = true;
									}
								}
							}
							for(Iterator<String> m = mailingAddress.getPostalStreet().iterator(); m.hasNext();) {
								String lineToAdd = m.next();
								if (lineToAdd != null) {
									lineToAdd = lineToAdd.trim();
									if(!lineToAdd.isEmpty()) {
										if(needsNewLine) mtpMailingAddress.addText(org.opencrx.kernel.utils.rtf.TextPart.NEWLINE);
										mtpMailingAddress.addText(new org.opencrx.kernel.utils.rtf.TextPart(lineToAdd));
										needsNewLine = true;
									}
								}
							}
							if(needsNewLine) mtpMailingAddress.addText(org.opencrx.kernel.utils.rtf.TextPart.NEWLINE);
							mtpMailingAddress.addText(new org.opencrx.kernel.utils.rtf.TextPart(
								(mailingAddress.getPostalCountry() == (short)0
									? ""
									: (String)(codes.getShortText(FEATURE_NAME_POSTAL_COUNTRY, app.getCurrentLocaleAsIndex(), true, true).get(new Short(mailingAddress.getPostalCountry()))) + "-") + (mailingAddress.getPostalCode() == null
										? ""
										: (mailingAddress.getPostalCode().length() > 0 ? mailingAddress.getPostalCode() + " " : "")) + (mailingAddress.getPostalCity() == null ? "" : mailingAddress.getPostalCity())
								));
						} catch (Exception e) {}
					} else {
						mtpMailingAddress.addText(new org.opencrx.kernel.utils.rtf.TextPart("WARNING: no postal address available"));
						mtpMailingAddress.addText(org.opencrx.kernel.utils.rtf.TextPart.NEWLINE);
					}
					// Filename
					String filenameDetail = "---";
					try {
						account = (Account)pm.getObjectById(mailingAddress.refGetPath().getParent().getParent());
						filenameDetail = ((account.getFullName() != null ? account.getFullName() : "---NoName")).replaceAll(" +", "_");
					} catch (Exception ef) {}
					String filename =
						this.getProviderName() + "_" + 
						this.getSegmentName() + "_" + 
						DECIMAL_FORMAT_6.format(counter) + "_" + 
						filenameDetail;
					filename = org.opencrx.kernel.utils.Utils.toFilename(filename) + ".rtf";
					filename = URLEncoder.encode(filename, "UTF-8");
					// Replace template bookmarks with actual values
					String suffix = recordIndex <= 0 ? "" : "" + Integer.toString(recordIndex);
					try {
						document.setBookmarkContent("filename" + suffix, filename, true);
					} catch (Exception e) {}
					try {
						document.setBookmarkContent("accountXri" + suffix, account.refGetPath().toXRI(), true);
					} catch (Exception e) {}
					try {
						document.setBookmarkContent("fullName" + suffix, account.getFullName(), true);
					} catch (Exception e) {}
					try {
						document.setBookmarkContent("counter" + suffix, DECIMAL_FORMAT_6.format(counter), true);
					} catch (Exception e) {}
					try {
						document.setBookmarkContent("timestamp" + suffix, timestamp.format(new java.util.Date()), true);
					} catch (Exception e) {}
					try {
						document.setBookmarkContent("addressXri" + suffix, mailingAddress.refGetPath().toXRI(), true);
					} catch (Exception e) {}
					try {
						document.setBookmarkContent("mailingAddress" + suffix, mtpMailingAddress, true);
					} catch (Exception e) {}
					if(bmTemplateRow == null) {
						zipos.putNextEntry(new ZipEntry(filename));
						document.writeTo(zipos);
						zipos.closeEntry();
					}
					if(bmTemplateRow != null) {
						recordIndex++;
					}
				}
				// Write file
				if(bmTemplateRow != null) {
					zipos.putNextEntry(new ZipEntry("MailMerge.rtf"));
					document.writeTo(zipos);
					zipos.closeEntry();
				}
				zipos.close();
				String downloadFileName = DOCUMENT_NAME + ".zip";
				this.downloadFileAction = new Action(
					Action.EVENT_DOWNLOAD_FROM_LOCATION,
					new Action.Parameter[]{
						new Action.Parameter(Action.PARAMETER_LOCATION, location),
						new Action.Parameter(Action.PARAMETER_NAME, downloadFileName),
						new Action.Parameter(Action.PARAMETER_MIME_TYPE, "application/zip")
					},
					app.getTexts().getClickToDownloadText() + " " + downloadFileName,
					true
				);
			} catch(Exception e) {
				new ServiceException(e).log();
			}
		}
	}

	/**
	 * @return the templateXri
	 */
	public String getTemplateXri() {
		return templateXri;
	}
	
	/**
	 * @return the downloadFileAction
	 */
	public Action getDownloadFileAction() {
		return downloadFileAction;
	}
	//-----------------------------------------------------------------------
	// Members
	//-----------------------------------------------------------------------
	private static final NumberFormat DECIMAL_FORMAT_6 = new DecimalFormat("000000");
	public static final String MAILMERGE_TEMPLATE_FOLDER_NAME = "Mail Merge Templates";
	public static final String TEMPLATE_MIMETYPE1 = "application/rtf";
	public static final String TEMPLATE_MIMETYPE2 = "text/rtf";
	public static final String FEATURE_NAME_POSTAL_COUNTRY = "country";
	public static final String DOCUMENT_NAME = "MailMerge";

	private String templateXri;
	private Action downloadFileAction;
}
