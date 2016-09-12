/*
 * ====================================================================
 * Project:     openCRX/core, http://www.opencrx.org/
 * Description: WebDavStore
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

/*
 * This source was originally published under net.sf.webdav.
 * 
 * Licensed under the Apache License, Version 2.0 (the "License");
 * you may not use this file except in compliance with the License.
 * You may obtain a copy of the License at
 *
 *      http://www.apache.org/licenses/LICENSE-2.0
 *
 * Unless required by applicable law or agreed to in writing, software
 * distributed under the License is distributed on an "AS IS" BASIS,
 * WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 * See the License for the specific language governing permissions and
 * limitations under the License.
 *
 */

package org.opencrx.application.uses.net.sf.webdav;

import java.io.InputStream;
import java.util.Collection;
import java.util.Date;
import java.util.List;

import javax.servlet.http.HttpServletRequest;
import javax.servlet.http.HttpServletResponse;

import org.opencrx.application.uses.net.sf.webdav.exceptions.LockFailedException;
import org.opencrx.application.uses.net.sf.webdav.exceptions.WebdavException;
import org.w3c.cci2.BinaryLargeObject;

/**
 * Interface for simple implementation of any store for the WebdavServlet
 * <p>
 * based on the BasicWebdavStore from Oliver Zeigermann, that was part of the
 * Webdav Construction Kit from slide
 * 
 */
public interface WebDavStore {

    /**
     * Indicates that a new request or transaction with this store involved has
     * been started. The request will be terminated by either 
     * {@link #commit(RequestContext transaction)} or
     * {@link #rollback(RequestContext transaction)}. If only non-read methods
     * have been called, the request will be terminated by a
     * {@link #commit(RequestContext transaction)}. This method will be
     * called by (@link WebdavStoreAdapter} at the beginning of each request.
     * 
     * 
     * @param principal
     *      the principal that started this request or <code>null</code> if
     *      there is non available
     * 
     * @throws WebdavException
     */
    RequestContext begin(
    	HttpServletRequest req,
    	HttpServletResponse resp
    );

    /**
     * Indicates that all changes done inside this request shall be made
     * permanent and any transactions, connections and other temporary resources
     * shall be terminated.
     * 
     * @param requestContext
     *      indicates that the method is within the scope of a WebDAV
     *      transaction
     * 
     * @throws WebdavException
     *      if something goes wrong on the store level
     */
    void commit(
    	RequestContext requestContext
    );

    /**
     * Indicates that all changes done inside this request shall be undone and
     * any transactions, connections and other temporary resources shall be
     * terminated.
     * 
     * @param requestContext
     *      indicates that the method is within the scope of a WebDAV
     *      transaction
     * 
     * @throws WebdavException
     *      if something goes wrong on the store level
     */
    void rollback(
    	RequestContext requestContext
    );

    /**
     * Creates a folder at the position specified by <code>folderUri</code>.
     * 
     * @param requestContext
     *      indicates that the method is within the scope of a WebDAV
     *      transaction
     * @param path
     *      URI of the folder
     * @throws WebdavException
     *      if something goes wrong on the store level
     */
    void createCollection(
    	RequestContext requestContext, 
    	String path
    );

    /**
     * Gets the content of the resource specified by <code>resourceUri</code>.
     * 
     * @param requestContext
     *      indicates that the method is within the scope of a WebDAV
     *      transaction
     * @param so
     *      content of the resource
     * @return input stream you can read the content of the resource from
     * @throws WebdavException
     *      if something goes wrong on the store level
     */
    
    public interface ResourceContent {
    	BinaryLargeObject getContent();
    	Long getLength();
    }

    ResourceContent getResourceContent(
    	RequestContext requestContext, 
    	Resource res
    );

    /**
     * Sets / stores the content of the resource specified by
     * <code>resourceUri</code>.
     * 
     * @param transaction
     *      indicates that the method is within the scope of a WebDAV
     *      transaction
     * @param resourceUri
     *      URI of the resource where the content will be stored
     * @param content
     *      input stream from which the content will be read from
     * @param contentType
     *      content type of the resource or <code>null</code> if unknown
     * @param characterEncoding
     *      character encoding of the resource or <code>null</code> if unknown
     *      or not applicable
     * @return length of resource
     * @throws WebdavException
     *      if something goes wrong on the store level
     */
    public enum Status {
    	OK, 
    	OK_CREATED, 
    	FORBIDDEN
    }

    Status putResource(
    	RequestContext transaction, 
    	String path,
        InputStream content, 
        String contentType 
    );

    /**
     * Gets the names of the children of the folder specified by
     * <code>folderUri</code>.
     * 
     * @param requestContext
     *      indicates that the method is within the scope of a WebDAV
     *      transaction
     * @param path
     *      URI of the collection
     * @param timeRangeStart include children starting from
     * @param timeRangeEnd include children end at
     * @return a (possibly empty) list of children
     * @throws WebdavException
     *      if something goes wrong on the store level
     */
    Collection<Resource> getChildren(
    	RequestContext requestContext, 
    	Resource res,
    	Date timeRangeStart,
    	Date timeRangeEnd
    );

    /**
     * Removes the object specified by <code>uri</code>.
     * 
     * @param requestContext
     *      indicates that the method is within the scope of a WebDAV
     *      transaction
     * @param path
     *      URI of the object, i.e. content resource or folder
     * @throws WebdavException
     *      if something goes wrong on the store level
     */
    Status removeResource(
    	RequestContext requestContext,
    	String path,
    	Resource res
    );

    /** 
     * Moves the resource from sourcePath to destinationPath
     * 
     * @param requestContext
     * @param res
     * @param sourcePath
     * @param destinationPath
     */
    Status moveResource(
    	RequestContext requestContext,
    	Resource res,
    	String sourcePath,
    	String destinationPath
    );

    /**
     * Gets the storedObject specified by <code>uri</code>
     * 
     * @param requestContext
     *      indicates that the method is within the scope of a WebDAV
     *      transaction
     * @param path
     *      URI
     * @return StoredObject
     */
    Resource getResourceByPath(
    	RequestContext requestContext, 
    	String path
    );

    /**
     * Gets mime type of stored object.
     */
    String getMimeType(Resource so);

    /**
     * Tries to lock the resource at "path".
     * 
     * @param requestContext
     * @param path
     *      what resource to lock
     * @param owner
     *      the owner of the lock
     * @param exclusive
     *      if the lock should be exclusive (or shared)
     * @param depth
     *      depth
     * @param timeout
     *      Lock Duration in seconds.
     * @return true if the resource at path was successfully locked, false if an
     *  existing lock prevented this
     * @throws LockFailedException
     */
    Lock lock(
    	RequestContext requestContext, 
    	String path, 
    	String token,
    	String owner,
        String scope, 
        String type, 
        int depth, 
        int timeout
    ) throws LockFailedException;

    /**
     * Unlocks all resources at "path" (and all subfolders if existing)<p/> that
     * have the same owner.
     * 
     * @param requestContext
     * @param id
     *      id to the resource to unlock
     */
    boolean unlock(
    	RequestContext requestContext,
    	String path,
    	String id 
    );

    /**
     * Gets the LockedObject on specified path.
     * 
     * @param requestContext
     * @param path
     *      Path to requested resource
     * @return LockedObject or null if no LockedObject on specified path exists
     */
    List<Lock> getLocksByPath(
    	RequestContext requestContext, 
    	String path
    );
    
}
