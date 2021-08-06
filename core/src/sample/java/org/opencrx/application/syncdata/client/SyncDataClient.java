/*
 * ====================================================================
 * Project:     openCRX/Core, http://www.opencrx.org/
 * Description: SyncDataClient
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
package org.opencrx.application.syncdata.client;

import java.io.ByteArrayOutputStream;
import java.io.File;
import java.io.FileOutputStream;
import java.io.IOException;
import java.nio.charset.StandardCharsets;
import java.sql.Connection;
import java.sql.DriverManager;
import java.sql.PreparedStatement;
import java.sql.SQLException;
import java.sql.Timestamp;
import java.util.Base64;
import java.util.Date;

import javax.jdo.PersistenceManager;
import javax.jdo.PersistenceManagerFactory;
import javax.naming.NamingException;

import org.opencrx.kernel.workflow1.jmi1.ImporterTask;
import org.opencrx.kernel.workflow1.jmi1.RunImportExportParams;
import org.opencrx.kernel.workflow1.jmi1.RunImportResult;
import org.openmdx.base.exception.ServiceException;
import org.openmdx.base.naming.Path;
import org.w3c.cci2.BinaryLargeObjects;
import org.w3c.spi2.Datatypes;
import org.w3c.spi2.Structures;

public class SyncDataClient {

	public static void main(
		String[] args
	) throws NamingException, ServiceException, IOException, SQLException {
		String connectionUrl = "http://127.0.0.1:8080/opencrx-rest-CRX/";
		String userName = "admin-Standard";
		String password = "admin-Standard";
		String databaseName = "test-activities.db";
		PersistenceManagerFactory pmf = org.opencrx.kernel.utils.Utils.getPersistenceManagerFactoryProxy(
			connectionUrl,
			userName,
			password,
			"application/vnd.openmdx.wbxml" // or 'text/xml' for plain xml protocol
		);
		PersistenceManager pm = pmf.getPersistenceManager(userName, null);
		org.opencrx.kernel.workflow1.jmi1.Segment workflowSegment =
			(org.opencrx.kernel.workflow1.jmi1.Segment)pm.getObjectById(
				new Path("xri://@openmdx*org.opencrx.kernel.workflow1/provider/CRX/segment/Standard")
			);
		// Read database
		File file = new File("build/tmp/" + databaseName);
		String fileContentBase64 = null;
		if(file.exists()) {
			// Add a new activity and set all existing to is_dirty=true
			{
				Connection conn = DriverManager.getConnection(String.format("jdbc:sqlite:%s", file.toPath().toString()));
				// Add new activity
				{
	            	PreparedStatement ps = conn.prepareStatement("insert into activity (name, description, scheduled_start, scheduled_end, due_by, priority, is_dirty) values(?, ?, ?, ?, ?, ?, ?)");
	            	ps.setString(1, SyncDataClient.class.getSimpleName() + " @ " + new Date());
	            	ps.setString(2, "Incident created by SyncDataTask @ " + new Date());
	            	ps.setTimestamp(3, new Timestamp(System.currentTimeMillis()));
	            	ps.setTimestamp(4, new Timestamp(System.currentTimeMillis() + 3600000L));
	            	ps.setTimestamp(5, new Timestamp(System.currentTimeMillis() + 3600000L));
	            	ps.setShort(6, Short.valueOf((short)0));
	            	ps.setBoolean(7, true);
	            	ps.executeUpdate();
	            	ps.close();
				}
            	// Set existing to is_dirty=true
				{
	            	PreparedStatement ps = conn.prepareStatement("update activity set is_dirty = true, description = 'Incident updated by SyncDataTask @ " + new Date() + "'");
	            	ps.executeUpdate();
	            	ps.close();
				}
				conn.close();
			}
			ByteArrayOutputStream fileContent = new ByteArrayOutputStream();
			BinaryLargeObjects.streamCopy(
				BinaryLargeObjects.valueOf(file).getContent(),
				0L,
				fileContent
			);
			fileContentBase64 = new String(Base64.getEncoder().encode(fileContent.toByteArray()), StandardCharsets.ISO_8859_1);
		}
		// Sync database
		pm.currentTransaction().begin();
		ImporterTask syncDataTask = (ImporterTask)workflowSegment.getWfProcess("SyncDataTask");
		RunImportExportParams syncDataParam = Structures.create(
			RunImportExportParams.class,
			Datatypes.member(RunImportExportParams.Member.param0, null),
			Datatypes.member(RunImportExportParams.Member.param1, fileContentBase64),
			Datatypes.member(RunImportExportParams.Member.param2, databaseName),	
			Datatypes.member(RunImportExportParams.Member.param3, "application/vnd.sqlite3"),
			Datatypes.member(RunImportExportParams.Member.param4, "org.opencrx.kernel.tasks/test-activities")
		);
		// Sync with sync profile "org.opencrx.kernel.tasks/test-activities"
		RunImportResult syncDataResult = syncDataTask.runImport(syncDataParam);
		pm.currentTransaction().commit();
		// Store database
		if(syncDataResult.getFile() != null) {
			BinaryLargeObjects.streamCopy(
				BinaryLargeObjects.valueOf(syncDataResult.getFile()).getContent(),
				0L,
				new FileOutputStream(file)
			);
		}
		System.out.println(String.format("status: %d", syncDataResult.getStatus()));
		System.out.println(String.format("statusMessage: %s", syncDataResult.getStatusMessage()));
		System.out.println(String.format("database: %s", file.toString()));
		pm.close();
	}

}
