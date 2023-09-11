/*
 * ====================================================================
 * Project:     openCRX/Gradle, http://www.opencrx.org/
 * Description: ClientJarTask
 * Owner:       the original authors.
 * ====================================================================
 *
 * This software is published under the BSD license
 * as listed below.
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
 * * Neither the name of the openCRX team nor the names of the contributors
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
package org.opencrx.gradle

import org.gradle.api.tasks.Input
import org.gradle.api.file.DuplicatesStrategy
import java.io.File
import org.gradle.kotlin.dsl.*

open class ClientJarTask : ArchiveTask() {

	init {
		from(
			project.layout.buildDirectory.dir("classes/java/main"),
			project.layout.buildDirectory.dir("src/main/resources"),
			"src/main/resources",
			getProject().zipTree(project.layout.buildDirectory.dir("generated/sources/model/opencrx-" + project.getName() + ".openmdx-xmi.zip"))
		)
		archiveFileName.set("opencrx-client.jar")
		destinationDirectory.set(File(deliverDir, "lib"))
	    includeEmptyDirs = false		
		include(
			"org/opencrx/**/cci2/**",
			"org/opencrx/**/jmi1/**",
			"org/opencrx/**/xmi1/**",
			"org/opencrx/kernel/utils/**",
			"org/opencrx/kernel/generic/**",
			"org/opencrx/kernel/generic/jpa3/**",
			"META-INF/openmdxmof.properties"
		)
		manifest {
	        attributes(
	        	getManifest(
	        		"openCRX/Client Library",
	        		"opencrx-client"
	        	)
	        )
	    }
		doLast {
			ant.withGroovyBuilder {
				"jar"(
					"destfile" to File(deliverDir, "lib/opencrx-client-sources.jar")
				) {
					"fileset"(
						"dir" to "src/main/java"
					) {
						"include"("name" to "org/opencrx/kernel/utils/**")
						"include"("name" to "org/opencrx/kernel/generic/**")
					}
					"fileset"(
						"dir" to project.layout.buildDirectory.dir("generated/sources/java/main").get().asFile
					) {
						"include"("name" to "org/opencrx/**/cci2/**")
						"include"("name" to "org/opencrx/**/jmi1/**")
						"include"("name" to "org/opencrx/**/xmi1/**")
						"include"("name" to "org/opencrx/kernel/generic/jpa3/**")
					}
				}
			}
		}
		
	}

}
