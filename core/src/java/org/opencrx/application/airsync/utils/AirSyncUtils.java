/*
 * ====================================================================
 * Project:     openCRX/Core, http://www.opencrx.org/
 * Description: AirSyncUtils
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
package org.opencrx.application.airsync.utils;

import java.io.ByteArrayInputStream;
import java.io.ByteArrayOutputStream;
import java.io.IOException;
import java.net.HttpURLConnection;
import java.net.URL;
import java.util.logging.Level;
import java.util.logging.Logger;

import javax.xml.transform.stream.StreamResult;

import org.opencrx.application.airsync.backend.cci.SyncBackend;
import org.opencrx.application.airsync.client.ClientHandler;
import org.opencrx.application.airsync.datatypes.AttachmentDataT;
import org.opencrx.application.airsync.server.SyncRequest;
import org.openmdx.base.exception.ServiceException;
import org.openmdx.base.io.QuotaByteArrayOutputStream;
import org.openmdx.kernel.exception.BasicException;
import org.w3c.cci2.BinaryLargeObjects;
import org.w3c.dom.Document;

public class AirSyncUtils {

	public static ClientHandler.SyncTarget newRemoteSyncTarget(
		final String serverUrl,
		final String username,
		final String domain,
		final String password,
		final String deviceId
	) {
		return new ClientHandler.SyncTarget(){

			@Override
            public Object perform(
            	String cmd,
            	String policyKey,
            	String userAgent,
            	Document requestDoc
            ) throws ServiceException {
				try {					
					URL url = new URL(serverUrl + "?Cmd=" + cmd + "&User=" + username + "&DeviceId=" + deviceId + "&DeviceType=OpenCrx");
					HttpURLConnection connection = (HttpURLConnection) url.openConnection();
					connection.setDoInput(true); 
					connection.setDoOutput(true);
			      	String userpassword = username + ":" + password;
			      	connection.setRequestProperty(
			      		"Authorization",
			      		"Basic " + org.openmdx.base.text.conversion.Base64.encode(userpassword.getBytes())
			      	);			
					connection.setRequestProperty(
						"Content-Type",
						"application/vnd.ms-sync.wbxml"
					);
					connection.setRequestProperty(
						"MS-ASProtocolVersion",
						"2.5"
					);
					if(policyKey != null) {
						connection.setRequestProperty(
							"X-Ms-Policykey",
							policyKey
						);
					}
					if(userAgent != null) {
						connection.setRequestProperty(
							"User-Agent",
							userAgent // userAgent
						);						
					}
					connection.setRequestProperty(
						"Accept",
						"*/*"
					);
					connection.setRequestMethod("POST");
					logger.log(Level.FINE, "+-+-+-+-+- Request +-+-+-+-+-");
					if(requestDoc == null) {
						connection.getOutputStream().close();
					}
					else {
						WbXMLTransformer.transformToWBXML(
							requestDoc,
							connection.getOutputStream()
						);
						if(logger.isLoggable(Level.FINE)) {
							ByteArrayOutputStream out = new ByteArrayOutputStream();
							System.out.println(url);
							WbXMLTransformer.transform(
								requestDoc,
								new StreamResult(out),
								false
							);
							out.close();
							logger.log(Level.FINE, out.toString());
						}
					}
					logger.log(Level.FINE, "POST Request", url);
					int responseCode = connection.getResponseCode();
					if(responseCode != HttpURLConnection.HTTP_OK) {
						throw new ServiceException(
							BasicException.Code.DEFAULT_DOMAIN,
							BasicException.Code.COMMUNICATION_FAILURE,
							"Unable to perform POST",
							new BasicException.Parameter("url", url),
							new BasicException.Parameter("status", responseCode)
						);
					}
					if(cmd.startsWith("GetAttachment")) {
						AttachmentDataT attachmentDataT = new AttachmentDataT();
						ByteArrayOutputStream content = new ByteArrayOutputStream();
						BinaryLargeObjects.streamCopy(connection.getInputStream(), 0, content);
						content.close();
						attachmentDataT.setContent(new ByteArrayInputStream(content.toByteArray()));
						attachmentDataT.setContentType(connection.getContentType());
						connection.disconnect();
						return attachmentDataT;
					}
					else {
						org.w3c.dom.Document responseBody = WbXMLTransformer.transformFromWBXML(
							connection.getInputStream()
						);
						connection.disconnect();
						return responseBody;
					}
				} catch(Exception e) {
					throw new ServiceException(e);
				}
            }
		};
	}
	
	public static ClientHandler.SyncTarget newLocalSyncTarget(
		final SyncBackend backend,
		final String profilePrefix,
		final String username,
		final String domain,
		final String deviceId
	) {
		return new ClientHandler.SyncTarget(){

			@Override
            public Object perform(
            	String cmd, 
            	String policyKey,
            	String userAgent,
            	Document requestDoc
            ) throws ServiceException {
				SyncRequest request = new SyncRequest(
					cmd,
					domain + SyncBackend.DOMAIN_SEPARATOR + username,
					deviceId					
				);
				// Normalize request
				ByteArrayOutputStream bos = new QuotaByteArrayOutputStream(AirSyncUtils.class.getName());
				try {
					WbXMLTransformer.transformToWBXML(
						requestDoc,
						bos
					);
					bos.close();
				} catch(IOException e) {
					throw new ServiceException(e);
				}
				requestDoc = WbXMLTransformer.transformFromWBXML(
					new ByteArrayInputStream(bos.toByteArray())
				);
				org.w3c.dom.Document responseDoc = null;
				if("Sync".equals(cmd)) {
					org.opencrx.application.airsync.server.SyncHandler syncHandler = 
						new org.opencrx.application.airsync.server.SyncHandler(backend, profilePrefix);
					responseDoc = syncHandler.handle(
						request, 
						requestDoc
					);					
				}
				else if("FolderSync".equals(cmd)) {
					org.opencrx.application.airsync.server.FolderSyncHandler syncHandler = 
						new org.opencrx.application.airsync.server.FolderSyncHandler(backend, profilePrefix);					
					responseDoc = syncHandler.handle(
						request, 
						requestDoc
					);									
				}
				else {
					throw new ServiceException(
						BasicException.Code.DEFAULT_DOMAIN,
						BasicException.Code.NOT_SUPPORTED,
						"Unsupported command",
						new BasicException.Parameter("command", cmd)
					);
				}
				// Normalize response
				bos.reset();
				try {
					WbXMLTransformer.transformToWBXML(
						responseDoc,
						bos
					);
					bos.close();
				} catch(IOException e) {
					throw new ServiceException(e);
				}
				responseDoc = WbXMLTransformer.transformFromWBXML(
					new ByteArrayInputStream(bos.toByteArray())
				);
				return responseDoc;				
            }			
		};
	}
		
	static final Logger logger = Logger.getLogger(AirSyncUtils.class.getPackage().getName());
	
}
