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
import java.sql.PreparedStatement;
import java.sql.ResultSet;
import java.sql.SQLException;
import java.sql.Statement;
import java.sql.Timestamp;
import java.util.Arrays;
import java.util.Base64;
import java.util.Collections;
import java.util.Date;

import javax.jdo.JDOHelper;
import javax.jdo.PersistenceManager;

import org.opencrx.kernel.activity1.cci2.IncidentQuery;
import org.opencrx.kernel.activity1.jmi1.Activity;
import org.opencrx.kernel.activity1.jmi1.ActivityCreator;
import org.opencrx.kernel.activity1.jmi1.ActivityTracker;
import org.opencrx.kernel.activity1.jmi1.Incident;
import org.opencrx.kernel.backend.Activities;
import org.opencrx.kernel.backend.Activities.ActivityClass;
import org.opencrx.kernel.backend.Workflows;
import org.opencrx.kernel.generic.jmi1.CrxObject;
import org.opencrx.kernel.utils.Utils;
import org.opencrx.kernel.workflow1.jmi1.ImporterTask;
import org.opencrx.kernel.workflow1.jmi1.RunImportResult;
import org.openmdx.base.accessor.jmi.cci.JmiServiceException;
import org.openmdx.base.exception.ServiceException;
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
		SCHEMA_NAME(4);

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

	public static enum Schema {
		
		TEST_ACTIVITIES("org.opencrx.kernel.tasks/test-activities");
		
		private Schema(
			String id
		) {
			this.id = id;
		}
		public String getId() {
			return this.id;
		}
		private final String id;
	}

	public static Date toDate(
		Timestamp value
	) {
		return value == null ? null : new Date(value.getTime());
	}
	
	public static Timestamp toTimestamp(
		Date value
	) {
		return value == null ? null : new Timestamp(value.getTime());
	}
	
	/**
	 * Sync data.
	 * 
	 * @param syncSource
	 * @param syncTarget
	 * @param statusMessage
	 * @param schemaName
	 * @return
	 * @throws SQLException
	 */
	public static short syncDataTest(
		Connection syncSource,
		CrxObject syncTarget,
		StringBuilder statusMessage,
		String schemaName
	) throws SQLException, ServiceException {
		PersistenceManager pm = JDOHelper.getPersistenceManager(syncTarget);
        if(Schema.TEST_ACTIVITIES.getId().equalsIgnoreCase(schemaName)) {
        	ActivityTracker activityTracker = (ActivityTracker)syncTarget;
        	ActivityCreator activityCreator = null;
        	for(ActivityCreator candidate: activityTracker.<ActivityCreator>getActivityCreator()) {
        		if(candidate.getActivityType().getActivityClass() == ActivityClass.INCIDENT.getValue()) {
        			activityCreator = candidate;
        			break;
        		}
        	}
        	Statement statement = syncSource.createStatement();
        	statement.executeUpdate("create table if not exists activity (object_xri string, activity_number string, name string, description string, scheduled_start timestamp, scheduled_end timestamp, due_by timestamp, priority short, created_at timestamp, modified_at timestamp, is_dirty boolean, is_disabled boolean)");
        	// Update existing activities with is_dirty=true
        	{
	            ResultSet rs = statement.executeQuery("select * from activity where object_xri is not null and is_dirty = true");
	            while(rs.next()) {
	            	String objectXri = rs.getString("object_xri");
	            	String name = rs.getString("name");
	            	String description = rs.getString("description");
	            	Timestamp scheduledStart = rs.getTimestamp("scheduled_start");
	            	Timestamp scheduledEnd = rs.getTimestamp("scheduled_end");
	            	Timestamp dueBy = rs.getTimestamp("due_by");
	            	Short priority = rs.getShort("priority");
	            	Boolean isDisabled = rs.getBoolean("is_disabled");
	            	SysLog.warning(String.format("Update activity: %s", objectXri));
	            	Activity activity = (Activity)pm.getObjectById(new Path(objectXri));
	            	activity.setName(name);
	            	activity.setDescription(description);
	            	activity.setScheduledStart(toDate(scheduledStart));
	            	activity.setScheduledEnd(toDate(scheduledEnd));
	            	activity.setDueBy(toDate(dueBy));
	            	activity.setPriority(priority == null ? (short)0 : priority);
	            	activity.setDisabled(isDisabled);
	            }
	            rs.close();
        	}
            // Create new activities
        	if(activityCreator != null) {
	            ResultSet rs = statement.executeQuery("select * from activity where object_xri is null and is_dirty = true");
	            while(rs.next()) {
	            	String name = rs.getString("name");
	            	String description = rs.getString("description");
	            	Timestamp scheduledStart = rs.getTimestamp("scheduled_start");
	            	Timestamp scheduledEnd = rs.getTimestamp("scheduled_end");
	            	Timestamp dueBy = rs.getTimestamp("due_by");
	            	Short priority = rs.getShort("priority");
	            	SysLog.warning(String.format("Create activity: %s", name));
	            	Activities.getInstance().newActivity(
    		            activityCreator,
    		            name,
    		            description,
    		            null, // detailedDescription,
    		            toDate(scheduledStart),
    		            toDate(scheduledEnd),
    		            dueBy,
    		            priority,
    		            null, // icalType,
    		            null, // icalClass
    		            null, // reportingContact,
    		            null, // creationContext,
    		            activityCreator.getActivityGroup()	            			
	            	);
	            }
	            rs.close();
        	}
        	// Truncate and load activities
        	{
	            statement.executeUpdate("delete from activity");
	            IncidentQuery incidentQuery = (IncidentQuery)pm.newQuery(Incident.class);
	            incidentQuery.forAllDisabled().isFalse();
	            for(Incident incident: activityTracker.<Incident>getFilteredActivity(incidentQuery)) {
	            	SysLog.warning(String.format("Return activity: %s", incident.refGetPath().toString()));	            	
	            	PreparedStatement ps = syncSource.prepareStatement("insert into activity (object_xri, activity_number, name, description, scheduled_start, scheduled_end, due_by, priority, created_at, modified_at, is_dirty) values(?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?)");
	            	ps.setString(1, incident.refGetPath().toString());
	            	ps.setString(2, incident.getActivityNumber());
	            	ps.setString(3, incident.getName());
	            	ps.setString(4, incident.getDescription());
	            	ps.setTimestamp(5, toTimestamp(incident.getScheduledStart()));
	            	ps.setTimestamp(6, toTimestamp(incident.getScheduledEnd()));
	            	ps.setTimestamp(7, toTimestamp(incident.getDueBy()));
	            	ps.setShort(8, incident.getPriority());
	            	ps.setTimestamp(9, toTimestamp(incident.getCreatedAt()));
	            	ps.setTimestamp(10, toTimestamp(incident.getModifiedAt()));
	            	ps.setBoolean(11, false);
	            	ps.executeUpdate();
	            	ps.close();
	            }
        	}
    		return 0;
        } else {
	        return Workflows.getInstance().syncData(
	        	syncSource,
	        	syncTarget,
	        	statusMessage,
	        	schemaName
	        );
        }
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
			String schemaName = params[Param.SCHEMA_NAME.getIndex()];
			byte[] fileContent = null;
			PersistenceManager pm = JDOHelper.getPersistenceManager(importerTask);
			StringBuilder statusMessage = new StringBuilder("");
			short status = 0;
			if(targetXri != null && !targetXri.isEmpty()) {
				CrxObject syncTarget = (CrxObject)pm.getObjectById(new Path(targetXri));
				java.nio.file.Path filePath = Files.createTempFile(null, "-" + Utils.getUidAsString() + ".db");
				SysLog.warning(String.format("file.path: %s", filePath.toString()));
				// Write database file
				if(fileContentBase64 != null && !fileContentBase64.isEmpty()) {
					fileContent = Base64.getDecoder().decode(fileContentBase64);
					SysLog.warning(String.format("file.size: %d", fileContent.length));
					FileOutputStream fileOs = new FileOutputStream(filePath.toFile());
					BinaryLargeObjects.streamCopy(BinaryLargeObjects.valueOf(fileContent).getContent(), 0L, fileOs);
					fileOs.close();
				} else {
					// Create empty database file
					new FileOutputStream(filePath.toFile()).close();
				}
				// Sync data
				{
		            Connection syncSource = null;
		            try {
		                syncSource = DriverManager.getConnection(String.format("jdbc:sqlite:%s", filePath.toString()));
	                	syncDataTest(
	                		syncSource,
	                		syncTarget,
	                		statusMessage,
	                		schemaName
	                	);
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
