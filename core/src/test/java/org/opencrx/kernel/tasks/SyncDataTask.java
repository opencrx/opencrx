/*
 * ====================================================================
 * Project:     openCRX/Core, http://www.opencrx.org/
 * Description: SyncDataTask
 * Owner:       CRIXP AG, Switzerland, http://www.crixp.com
 * ====================================================================
 *
 * This software is published under the BSD license
 * as listed below.
 * 
 * Copyright (c) 2021 CRIXP Corp., Switzerland
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

package org.opencrx.kernel.tasks;

import java.io.ByteArrayOutputStream;
import java.io.FileOutputStream;
import java.nio.file.Files;
import java.sql.Connection;
import java.sql.DriverManager;
import java.sql.ResultSet;
import java.sql.SQLException;
import java.sql.Statement;
import java.util.Arrays;
import java.util.Base64;

import javax.jdo.JDOHelper;
import javax.jdo.PersistenceManager;

import org.opencrx.kernel.backend.Workflows;
import org.opencrx.kernel.generic.jmi1.CrxObject;
import org.opencrx.kernel.utils.Utils;
import org.opencrx.kernel.workflow1.jmi1.ImporterTask;
import org.opencrx.kernel.workflow1.jmi1.RunImportResult;
import org.openmdx.base.accessor.jmi.cci.JmiServiceException;
import org.openmdx.base.naming.Path;
import org.openmdx.kernel.log.SysLog;
import org.w3c.cci2.BinaryLargeObjects;
import org.w3c.spi2.Datatypes;
import org.w3c.spi2.Structures;

public class SyncDataTask {
	
	public static enum Param {
	
		TARGET_XRI(0),
		FILE(1),
		FILE_NAME(2),
		FILE_MIME_TYPE(3),
		MODE(4);

		private Param(
			int index
		) {
			this.index = index;
		}
	
		public int getIndex(
		) {
			return this.index;
		}
	
		private final int index;
	}

	/**
	 * Test sync data.
	 * 
	 * @param syncSource
	 * @param syncTarget
	 * @param statusMessage
	 * @return
	 * @throws SQLException
	 */
	public static short syncDataTest(
		Connection syncSource,
		CrxObject syncTarget,
		StringBuilder statusMessage
	) throws SQLException {
        {
        	Statement statement = syncSource.createStatement();
            ResultSet rs = statement.executeQuery("select * from person");
            while(rs.next())
            {
              // read the result set
              SysLog.warning("name = " + rs.getString("name"));
              SysLog.warning("id = " + rs.getInt("id"));
            }                	
        }		
		return 0;
	}

	/**
	 * SyncDataTask
	 * 
	 * @param in
	 * @return
	 */
	public static RunImportResult runImport(
		ImporterTask importerTask,
		String[] params
	) {
		try {
			SysLog.warning("params=" + Arrays.asList(params));
			String targetXri = params[Param.TARGET_XRI.getIndex()];
			String fileContentBase64 = params[Param.FILE.getIndex()];
			String fileName = params[Param.FILE_NAME.getIndex()];
			String fileMimeType = params[Param.FILE_MIME_TYPE.getIndex()];
			String mode = params[Param.MODE.getIndex()];
			byte[] fileContent = null;
			PersistenceManager pm = JDOHelper.getPersistenceManager(importerTask);
			StringBuilder statusMessage = new StringBuilder("");
			short status = 0;
			if(targetXri != null && !targetXri.isEmpty()) {
				CrxObject syncTarget = (CrxObject)pm.getObjectById(new Path(targetXri));
				if(fileContentBase64 != null && !fileContentBase64.isEmpty()) {
					java.nio.file.Path filePath = Files.createTempFile(null, "-" + Utils.getUidAsString());
					SysLog.warning(String.format("file.path: %s", filePath.toString()));
					// Write database file
					{
						fileContent = Base64.getDecoder().decode(fileContentBase64);
						SysLog.warning(String.format("file.size: %d", fileContent.length));
						FileOutputStream fileOs = new FileOutputStream(filePath.toFile());
						BinaryLargeObjects.streamCopy(BinaryLargeObjects.valueOf(fileContent).getContent(), 0L, fileOs);
						fileOs.close();
					}
					// Sync data
					{
			            Connection syncSource = null;
			            try {
			                syncSource = DriverManager.getConnection(String.format("jdbc:sqlite:%s", filePath.toString()));
			                if("test".equalsIgnoreCase(mode)) {
			                	syncDataTest(
			                		syncSource,
			                		syncTarget,
			                		statusMessage
			                	);
			                } else {
				                status = Workflows.getInstance().syncData(
				                	syncSource,
				                	syncTarget,
				                	statusMessage
				                );
			                }
			            } finally {
			                try {
				                if(syncSource != null) {
				                    syncSource.close();
				                }
			                } catch(Exception ignore) {}
			            }
					}
		            // Read database file
		            {
		            	ByteArrayOutputStream fileOs = new ByteArrayOutputStream();
		            	BinaryLargeObjects.streamCopy(BinaryLargeObjects.valueOf(filePath.toFile()).getContent(), 0L, fileOs);
		            	fileOs.close();
		            	fileContent = fileOs.toByteArray();
		            }
				} else {
					statusMessage.append(String.format("ERROR: Parameter %s must be a base64 encoded sqlite database file", Param.FILE.name()));
					status = -1;
				}
			} else {
				statusMessage.append(String.format("ERROR: Parameter %s must be XRI of an existing object", Param.TARGET_XRI.name()));
				status = -1;
			}
			return (RunImportResult)Structures.create(
				RunImportResult.class,
				Datatypes.member(org.opencrx.kernel.workflow1.cci2.RunImportResult.Member.importedObject, null),
				Datatypes.member(org.opencrx.kernel.workflow1.cci2.RunImportResult.Member.file, fileContent),
				Datatypes.member(org.opencrx.kernel.workflow1.cci2.RunImportResult.Member.fileName, fileName),
				Datatypes.member(org.opencrx.kernel.workflow1.cci2.RunImportResult.Member.fileMimeType, fileMimeType),
				Datatypes.member(org.opencrx.kernel.workflow1.cci2.RunImportResult.Member.status, (short)status),
				Datatypes.member(org.opencrx.kernel.workflow1.cci2.RunImportResult.Member.statusMessage, statusMessage.toString())
			);
		} catch (Exception e) {
			throw new JmiServiceException(e);
		} finally {
		}
	}

}
