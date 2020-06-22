/*
 * ====================================================================
 * Project:     openCRX/core, http://www.opencrx.org/
 * Description: WebdavStatus
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

public class WebdavStatus {

    /**
     * Status code (207) indicating that the response requires providing status
     * for multiple independent operations.
     */
    public static final int SC_MULTI_STATUS = 207;

    // This one colides with HTTP 1.1
    // "207 Parital Update OK"

    /**
     * Status code (418) indicating the entity body submitted with the PATCH
     * method was not understood by the resource.
     */
    public static final int SC_UNPROCESSABLE_ENTITY = 418;

    // This one colides with HTTP 1.1
    // "418 Reauthentication Required"

    /**
     * Status code (419) indicating that the resource does not have sufficient
     * space to record the state of the resource after the execution of this
     * method.
     */
    public static final int SC_INSUFFICIENT_SPACE_ON_RESOURCE = 419;

    // This one colides with HTTP 1.1
    // "419 Proxy Reauthentication Required"

    /**
     * Status code (420) indicating the method was not executed on a particular
     * resource within its scope because some part of the method's execution
     * failed causing the entire method to be aborted.
     */
    public static final int SC_METHOD_FAILURE = 420;

    /**
     * Status code (423) indicating the destination resource of a method is
     * locked, and either the request did not contain a valid Lock-Info header,
     * or the Lock-Info header identifies a lock held by another principal.
     */
    public static final int SC_LOCKED = 423;

}
