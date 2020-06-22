/*
 * ====================================================================
 * Project:     openCRX/Application, http://www.opencrx.org/
 * Description: Sync for openCRX
 * Owner:       CRIXP AG, Switzerland, http://www.crixp.com
 * ====================================================================
 *
 * This software is published under the BSD license
 * as listed below.
 * 
 * Copyright (c) 2010, CRIXP Corp., Switzerland
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
package org.opencrx.application.airsync.backend.cci;

import java.io.File;
import java.io.InputStream;
import java.util.List;
import java.util.Set;

import org.opencrx.application.airsync.datatypes.AttachmentDataT;
import org.opencrx.application.airsync.datatypes.IData;
import org.opencrx.application.airsync.datatypes.SyncCollection;
import org.opencrx.application.airsync.datatypes.SyncDataItem;
import org.opencrx.application.airsync.datatypes.SyncFolder;
import org.openmdx.base.exception.ServiceException;

public interface SyncBackend {

	public static final String DOMAIN_SEPARATOR = "\\";

	public interface RequestContext {
	
		public String getUserId();		
		public String getSyncKey();
		public Object getContext();
	}
	
	public RequestContext newRequestContext(
		final String userId,
		final Object context
	);
	
	public String getNextSyncKey(
		RequestContext requestContext,
		String syncKey
	) throws ServiceException;
	
	public SyncDataItem fetchDataItem(
		RequestContext requestContext,
		String profileName,
		SyncCollection collection,
		String itemId
	) throws ServiceException;
	
	public String moveDataItem(
		RequestContext requestContext,
		String profileName,
		String srcFolderId,
		String dstFolderId,
		String itemId
	) throws ServiceException;

	public AttachmentDataT getAttachementData(
		RequestContext requestContext,
		String attachmentId
	) throws ServiceException;

	public String createOrUpdateDataItem(
		RequestContext requestContext,
		String profileName,
		SyncCollection collection,
		String itemId, 
		IData data
	) throws ServiceException;
	
    public void deleteDataItem(
    	RequestContext requestContext,
    	String profileName,
    	SyncCollection collection,
    	String itemId
    ) throws ServiceException;

	public void setDataItemReadFlag(
		RequestContext requestContext,
		String folderId,
		String itemId, 
		boolean read
	) throws ServiceException;

	public GetChangedDataItemsResult getChangedDataItems(
		RequestContext requestContext,
		String profileName,
		SyncCollection collection,
		boolean noData,
		int maxItems,
    	SyncDataItem.State state,
    	Set<String> excludes
	) throws ServiceException;
	
	/**
	 * Return deleted data items starting from collection.getSyncKey() up to syncKeyTo
	 *   
	 * @param requestContext
	 * @param profileName
	 * @param collection
	 * @param currentSyncKey
	 * @return list of deleted items
	 * @throws ServiceException
	 */
	public List<String> getDeletedDataItems(
		RequestContext requestContext,
		String profileName,
		SyncCollection collection,
		String syncKeyTo
	) throws ServiceException;
	
	public List<SyncFolder> getChangedFolders(
		RequestContext requestContext,
		String profileName,
		String syncKey
	) throws ServiceException;
	
	public List<SyncFolder> getDeletedFolders(
		RequestContext requestContext,
		String profileName,
		String syncKey
	) throws ServiceException;
	
	public String createOrUpdateFolder(
		RequestContext requestContext,
		SyncFolder folder
	) throws ServiceException;
	
	public String deleteFolder(
		RequestContext requestContext,
		String folderId
	) throws ServiceException;	

	/**
	 * Validates whether folder specified by folderId and collectionType is valid.
	 * @param userId
	 * @param folderId
	 * @return true if folderId is valid.
	 */
	public boolean folderIsValid(
		RequestContext requestContext,
		String profileName,
		SyncCollection collection
	) throws ServiceException;
	
	public void sendMail(
		RequestContext requestContext,
		InputStream mimeMessage
	) throws ServiceException;
	
	public File getContextTempDir(
		RequestContext requestContext,
		File tempDir
	) throws ServiceException;

	public ClientProfile getClientProfile(
		RequestContext requestContext,
		String profileName
	) throws ServiceException;
	
	/**
	 * Updates the client profile. 
	 * 
	 * @param requestContext request context.
	 * @param clientProfile profile.
	 * @param folderIds list of folders to be updated. If null all folders are updated.
	 * @param noSyncKeys sync keys are not updated if true.
	 * @param noMappings mappings are not updated if true.
	 * @throws ServiceException
	 */
	public void updateClientProfile(
		RequestContext requestContext,
		ClientProfile clientProfile,
		Set<String> folderIds,
		boolean noSyncKeys,
		boolean noMappings
	) throws ServiceException;
			
}
