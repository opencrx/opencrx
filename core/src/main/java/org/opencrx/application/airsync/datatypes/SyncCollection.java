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
package org.opencrx.application.airsync.datatypes;

import java.io.Serializable;

import org.opencrx.application.airsync.utils.DOMUtils;
import org.w3c.dom.Element;

public class SyncCollection implements Serializable {
	
    private static final long serialVersionUID = -502421818332948856L;

    public static final Integer SYNC_TRUNCATION_ALL = 9;

	public SyncCollection() {
		conflict = 1;
		collectionId = null;
		truncation = SyncCollection.SYNC_TRUNCATION_ALL;
		moreAvailable = false;
		windowSize = 100;
	}
	
	public boolean isImportedChanges(
	) {
    	return importedChanges;
    }

	public void setImportedChanges(
		boolean importedChanges
	) {
    	this.importedChanges = importedChanges;
    }

	public Integer getMaxItems(
	) {
    	return maxItems;
    }

	public void setMaxItems(
		Integer maxItems
	) {
    	this.maxItems = maxItems;
    }

	public boolean isGetChanges(
	) {
    	return getChanges;
    }

	public void setGetChanges(
		boolean getChanges
	) {
    	this.getChanges = getChanges;
    }

	public DataType getDataType(
	) {
		return dataType;
	}
	
	public void setDataType(
		DataType dataType
	) {
		this.dataType = dataType;
	}
	
	public Integer getConflict(
	) {
		return conflict;
	}
	
	public void setConflict(
		Integer conflict
	) {
		this.conflict = conflict;
	}
	
	public String getCollectionId(
	) {
		return collectionId;
	}
	
	public void setCollectionId(
		String collectionId
	) {
		this.collectionId = collectionId;
	}
	
	public String getSyncKey() {
		return syncKey;
	}
	
	public void setSyncKey(String syncKey) {
		this.syncKey = syncKey;
	}

	public Integer getTruncation() {
		return truncation;
	}

	public void setTruncation(Integer truncation) {
		this.truncation = truncation;
	}

	public boolean isDeletesAsMoves() {
		return deletesAsMoves;
	}

	public void setDeletesAsMoves(boolean deletesAsMoves) {
		this.deletesAsMoves = deletesAsMoves;
	}

	public FilterType getFilterType() {
		return filterType;
	}

	public void setFilterType(FilterType filterType) {
		this.filterType = filterType;
	}
	
	public Integer getWindowSize() {
		return windowSize;
	}

	public void setWindowSize(Integer windowSize) {
		this.windowSize = windowSize;
	}
	
	public boolean isMoreAvailable() {
		return moreAvailable;
	}

	public void setMoreAvailable(boolean moreAvailable) {
		this.moreAvailable = moreAvailable;
	}
	
	@Override
	public boolean equals(Object obj) {
		return collectionId.equals(((SyncCollection) obj).collectionId);
	}

	@Override
	public int hashCode() {
		return collectionId.hashCode();
	}

	public Integer getMimeSupport() {
		return mimeSupport;
	}

	public void setMimeSupport(Integer mimeSupport) {
		this.mimeSupport = mimeSupport;
	}

	public Integer getMimeTruncation() {
		return mimeTruncation;
	}

	public void setMimeTruncation(Integer mimeTruncation) {
		this.mimeTruncation = mimeTruncation;
	}
	
	public BodyPreference getBodyPreference() {
		return bodyPreference;
	}

	public void setBodyPreference(BodyPreference bodyPreference) {
		this.bodyPreference = bodyPreference;
	}

	public static SyncCollection decode(
		String namespace,
		Element e
	) {
		SyncCollection collection = new SyncCollection();
		String collectionClass = DOMUtils.getElementText(e, "AirSync:", "Class");
		if(collectionClass == null) {
			collectionClass = DOMUtils.getElementText(e, namespace, "Class");
		}
		if(collectionClass != null) {
			collection.setDataType(			
				DataType.valueOf(collectionClass)
			);
		}
		collection.setSyncKey(DOMUtils.getElementText(e, "AirSync:", "SyncKey"));
		Element eCollectionId = DOMUtils.getUniqueElement(
			e, 
			namespace == null ? "AirSync:" : namespace, 
			"CollectionId"
		);
		if(eCollectionId != null) {
			collection.setCollectionId(eCollectionId.getTextContent());
		}
		Element eFilterType = DOMUtils.getUniqueElement(e, "AirSync:", "FilterType");
		if(eFilterType != null) {
			collection.setFilterType(FilterType.toFilterType(Integer.parseInt(eFilterType.getTextContent())));
		}
		Element wse = DOMUtils.getUniqueElement(e, "AirSync:", "WindowSize");
		if(wse != null) {
			collection.setWindowSize(Integer.parseInt(wse.getTextContent()));
		}
		collection.setDeletesAsMoves(
			DOMUtils.getUniqueElement(e, "AirSync:", "DeletesAsMoves") != null
		);
		collection.setGetChanges(
			DOMUtils.getUniqueElement(e, "AirSync:", "GetChanges") != null
		);
		Element maxItems = DOMUtils.getUniqueElement(e, "AirSync:", "MaxItems");
		if(maxItems != null) {
			collection.setMaxItems(Integer.parseInt(maxItems.getTextContent()));
		}
		Element option = DOMUtils.getUniqueElement(e, "AirSync:", "Options");
		if(option != null) {
			String filterType = DOMUtils.getElementText(option, "AirSync:", "FilterType");
			String truncation = DOMUtils.getElementText(option, "AirSync:", "Truncation");
			String mimeSupport = DOMUtils.getElementText(option, "AirSync:", "MIMESupport");
			String mimeTruncation = DOMUtils.getElementText(option, "AirSync:", "MIMETruncation");
			String conflict = DOMUtils.getElementText(option, "AirSync:", "Conflict");
			Element bodyPreference = DOMUtils.getUniqueElement(e, "AirSync:", "BodyPreference");
			if(conflict != null) {
				collection.setConflict(Integer.parseInt(conflict));
			}
			if(filterType != null) {
				collection.setFilterType(FilterType.toFilterType(Integer.parseInt(filterType)));
			}
			if(mimeSupport != null) {
				collection.setMimeSupport(Integer.parseInt(mimeSupport));
			}
			if(mimeTruncation != null) {
				collection.setMimeTruncation(Integer.parseInt(mimeTruncation));
			}
			if(truncation != null) {
				collection.setTruncation(Integer.parseInt(truncation));
			}
			if(bodyPreference != null) {
				String truncationSize = DOMUtils.getElementText(bodyPreference, "AirSync:", "TruncationSize");
				String type = DOMUtils.getElementText(bodyPreference, "AirSync:", "Type");
				BodyPreference bp = new BodyPreference();
				// nokia n900 sets type without truncation size
				if(truncationSize != null) {
					bp.setTruncationSize(Integer.parseInt(truncationSize));
				}
				bp.setType(MimeType.toEmailBodyType(Integer.parseInt(type)));
				collection.setBodyPreference(bp);
			}
		}
		return collection;
	}

	//-----------------------------------------------------------------------
    // Members
    //-----------------------------------------------------------------------
	private DataType dataType;
	private Integer conflict;
	private String collectionId;
	private String syncKey;
	private Integer truncation;
	private boolean deletesAsMoves;
	private boolean getChanges;
	private boolean importedChanges;
	private FilterType filterType;
	private Integer windowSize;
	private boolean moreAvailable;
	private Integer mimeSupport;
	private Integer mimeTruncation;
	private BodyPreference bodyPreference;
	private Integer maxItems;
		
}
