/*
 * ====================================================================
 * Project:     openCRX/Core, http://www.opencrx.org/
 * Description: ScriptUtils
 * Owner:       CRIXP AG, Switzerland, http://www.crixp.com
 * ====================================================================
 *
 * This software is published under the BSD license
 * as listed below.
 * 
 * Copyright (c) 2004-2014, CRIXP Corp., Switzerland
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
package org.opencrx.kernel.utils;

import java.util.Arrays;
import java.util.List;
import java.util.Map;

import org.codehaus.commons.compiler.CompileException;
import org.codehaus.janino.ClassBodyEvaluator;
import org.openmdx.base.exception.ServiceException;
import org.openmdx.kernel.exception.BasicException;

/**
 * ScriptUtils
 *
 */
public class ScriptUtils {

	/**
	 * Get compiled class for script.
	 * 
	 * @param script
	 * @return
	 * @throws ServiceException
	 */
	public static Class<?> getClass(
		String script
	) throws ServiceException {
		try {
			for(String invalidWord: INVALID_WORDS) {
				if(script.indexOf(invalidWord) >= 0) {
					throw new ServiceException(
						BasicException.Code.DEFAULT_DOMAIN,
						BasicException.Code.ASSERTION_FAILURE,
						"Script contains invalid words",
						new BasicException.Parameter("script", script),
						new BasicException.Parameter("word", invalidWord),
						new BasicException.Parameter("invalid words", INVALID_WORDS)
					);
				}
			}
			Map<String,Class<?>> classes = compiledScripts.get();
			Class<?> clazz = classes.get(script);
			if(clazz == null) {
				clazz = new ClassBodyEvaluator(script).getClazz();	
				classes.put(
					script,
					clazz
				);
			}
			return clazz;
		} catch(CompileException e) {
        	throw new ServiceException(
        		e,
        		BasicException.Code.DEFAULT_DOMAIN,
        		BasicException.Code.PARSE_FAILURE,
        		"Script compile error",
        		new BasicException.Parameter("script", script)
        	);
        } catch(Exception e) {
			throw new ServiceException(e);
		}
	}

	private static final List<String> INVALID_WORDS = Arrays.asList(
		"commit(", "commit ",
		"begin(", "begin ",
		"getPersistenceManagerFactory",
		"System.", "System ",
		"InitialContext"
	);

    protected static final ThreadLocal<Map<String,Class<?>>> compiledScripts = new ThreadLocal<Map<String,Class<?>>>() {
        protected synchronized Map<String,Class<?>> initialValue() {
            return new java.util.IdentityHashMap<String,Class<?>>();
        }         
    };

}
