/*
 * ====================================================================
 * Project:     openCRX/Core, http://www.opencrx.org/
 * Description: openCRX application plugin
 * Owner:       CRIXP AG, Switzerland, http://www.crixp.com
 * ====================================================================
 *
 * This software is published under the BSD license
 * as listed below.
 * 
 * Copyright (c) 2004-2007, CRIXP Corp., Switzerland
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

import java.util.ArrayList;
import java.util.Iterator;
import java.util.List;
import java.util.Map;
import java.util.Properties;

import org.opencrx.application.airsync.datatypes.FolderType;

public class ClientProfile {

	public static class Folder {

		public static class ItemIdMapping {
			
			public ItemIdMapping(
				String clientId,
				String serverId
			) {
				this.clientId = clientId;
				this.serverId = serverId;
			}
			
			public String getClientId() {
	        	return clientId;
	        }
			public String getServerId() {
	        	return serverId;
	        }

			private final String clientId;
			private final String serverId;
		}
		
		public class ItemIdMappings {
		
			public ItemIdMapping getMappingByClientId(
				String serverId
			) {
				for(Map.Entry<Object,Object> e: Folder.this.idMap.entrySet()) {
					if(serverId.equals(e.getValue())) {
						String key = (String)e.getKey();
						// Cut off generation index
						String clientId = key.indexOf("{") > 0 ?
							key.substring(0, key.indexOf("{")) :								
								key;
						return new ItemIdMapping(
							clientId,
							serverId
						);
					}
				}
				return null;
			}
			
			public String getServerId(
				String clientId
			) {
				for(int g = Folder.this.generation; g >= -1; g--) {
					String serverId = Folder.this.idMap.getProperty(clientId + (g < 0 ? "" : "{" + g + "}"));
					if(serverId != null) {
						return serverId;
					}
				}
				return null;
			}

			public void removeAllMappingsByClientId(
				String clientId
			) {
				for(Iterator<Object> i = Folder.this.idMap.keySet().iterator(); i.hasNext(); ) {
					String key = (String)i.next();
					if(key.startsWith(clientId)) {
						i.remove();
					}
				}
			}
			
			public void removeAllMappingsByServerId(
				String serverId
			) {
				for(Iterator<Map.Entry<Object,Object>> i = Folder.this.idMap.entrySet().iterator(); i.hasNext(); ) {
					Map.Entry<Object,Object> e = i.next();
					if(serverId.equals(e.getValue())) {
						i.remove();
					}
				}			
			}

			public void updateMappings(
				String clientId,
				String serverId
			) {			
				this.removeAllMappingsByClientId(clientId);
				String key = clientId + "{" + Folder.this.generation + "}";
				Folder.this.idMap.put(key, serverId);
			}

			public List<ItemIdMapping> getOldMappings(
			) {
				List<ItemIdMapping> oldMappings = new ArrayList<ItemIdMapping>();
				for(Map.Entry<Object,Object> e: Folder.this.idMap.entrySet()) {
					String key = (String)e.getKey();
					int generation = Folder.this.getGeneration(key);
					if(generation < Folder.this.generation) {
						int pos = key.indexOf("{");
						oldMappings.add(
							new ItemIdMapping(
								pos > 0 ? key.substring(0, pos) : key,
								(String)e.getValue()
							)
						);
					}
				}
				return oldMappings;
			}
						
		}
		
		public String getClientId() {
	    	return clientId;
	    }

		public void setClientId(String clientId) {
	    	this.clientId = clientId;
	    }

		public String getServerId() {
	    	return serverId;
	    }

		public void setServerId(String serverId) {
	    	this.serverId = serverId;
	    }

		public String getSyncKeyServer() {
	    	return syncKeyServer;
	    }

		public void setSyncKeyServer(String syncKeyServer) {
	    	this.syncKeyServer = syncKeyServer;
	    }

		public String getSyncKeyClient() {
	    	return syncKeyClient;
	    }

		public void setSyncKeyClient(String syncKeyClient) {
	    	this.syncKeyClient = syncKeyClient;
	    }

		public FolderType getType() {
        	return type;
        }

		public void setType(FolderType type) {
        	this.type = type;
        }

		public String getName() {
        	return name;
        }

		public void setName(String name) {
        	this.name = name;
        }

		public String getParentId() {
        	return parentId;
        }

		public void setParentId(String parentId) {
        	this.parentId = parentId;
        }

		public int getGeneration() {
        	return generation;
        }

		public void setGeneration(int generation) {
        	this.generation = generation;
        }
		
		public Properties getIdMap() {
        	return idMap;
        }

		public void setIdMap(Properties idMap) {
        	this.idMap = idMap;
        }
		
		protected int getGeneration(
			String key
		) {
			int pos1 = key.indexOf("{");
			int pos2 = key.indexOf("}");
			int generation = 0;
			if(pos1 > 0 && pos2 > pos1) {
				generation = Integer.valueOf(key.substring(pos1 + 1, pos2));
			}
			return generation;
		}
		
		public ItemIdMappings getItemIdMappings(
		) {
			return new ItemIdMappings();
		}
		
		private String name = null;
		protected int generation = 0;
		private String clientId = null;
		private String parentId = null;
		private String serverId = null;
		private String syncKeyServer = null;
		private String syncKeyClient = null;
		private FolderType type = null;
		protected Properties idMap = null;
		
	}
		
	public List<Folder> getFolders() {
    	return folders;
    }
	public void setFolders(List<Folder> folders) {
    	this.folders = folders;
    }
	public String getUserId() {
    	return userId;
    }
	public void setUserId(String userId) {
    	this.userId = userId;
    }
	public String getName() {
    	return name;
    }
	public void setName(String name) {
    	this.name = name;
    }
	public String getPolicyKey() {
    	return policyKey;
    }
	public void setPolicyKey(String policyKey) {
    	this.policyKey = policyKey;
    }
	public String getUserAgent() {
    	return userAgent;
    }
	public void setUserAgent(String userAgent) {
    	this.userAgent = userAgent;
    }
	private String userId = null;
	private String name = null;
	private String policyKey = null;
	private String userAgent = null;
	private List<Folder> folders = null;
	
}
