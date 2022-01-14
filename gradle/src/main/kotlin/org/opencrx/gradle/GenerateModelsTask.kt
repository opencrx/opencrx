/*
 * ====================================================================
 * Project:     openCRX/Gradle, http://www.opencrx.org/
 * Description: GenerateModelsTask
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

import org.gradle.api.tasks.bundling.Zip
import org.gradle.api.tasks.Input
import java.io.File
import org.gradle.kotlin.dsl.*

open class GenerateModelsTask : ExecTask() {

	init {
		main = "org.openmdx.application.mof.externalizer.xmi.XMIExternalizer"
		args = listOf(
			"--pathMapSymbol=openMDX 2 ~ Core (EMF)",
			"--pathMapPath=file:" + File(project.getBuildDir(), "generated/sources/model/openmdx/base") + "/",
			"--pathMapSymbol=openMDX 2 ~ Security (EMF)",
			"--pathMapPath=file:" + File(project.getBuildDir(), "generated/sources/model/openmdx/security") + "/",
			"--pathMapSymbol=openMDX 2 ~ Portal (EMF)",
			"--pathMapPath=file:" + File(project.getBuildDir(), "generated/sources/model/openmdx/portal") + "/",
			"--pathMapSymbol=openCRX 4 ~ Core (EMF)",
			"--pathMapPath=file:" + File(project.getBuildDir(), "generated/sources/model/opencrx/core") + "/",
			"--pathMapSymbol=openCRX 5 ~ Core (EMF)",
			"--pathMapPath=file:" + File(project.getBuildDir(), "generated/sources/model/opencrx/core") + "/",
			"--url=file:src/model/emf/models.uml",
			"--xmi=emf",
			"--out=" + File(project.getBuildDir(), "generated/sources/model/opencrx-" + project.getName() + "-models.zip"),
			"--openmdxjdo=" + File(project.getProjectDir(), "src/main/resources"),
			"--dataproviderVersion=2",
			"--format=xmi1",
			"--format=cci2",
			"--format=jmi1",
			"--format=jpa3",
			"%"
		)
		doLast {
			ant.withGroovyBuilder {
				"zip"(
					"destfile" to File(project.getBuildDir(), "generated/sources/model/opencrx-" + project.getName() + ".openmdx-xmi.zip")
				) {
					"zipfileset"(
						"src" to File(project.getBuildDir(), "generated/sources/model/opencrx-" + project.getName() + "-models.zip")
					) {
						"include"("name" to "**/*.xsd")
						"include"("name" to "**/*.xml")
						"include"("name" to "**/*.wbxml")
					}
				}
			}
			ant.withGroovyBuilder {
				"zip"(
					"destfile" to File(project.getBuildDir(), "generated/sources/model/opencrx-" + project.getName() + ".openmdx-emf.zip"),
					"basedir" to File(project.getProjectDir(), "src/model/emf")
				)
			}
			project.copy {
				from(project.zipTree(File(project.getBuildDir(), "generated/sources/model/opencrx-" + project.getName() + "-models.zip"))) { include("META-INF/") }
				into(File(project.getBuildDir(), "resources/main"))
			}
		}
		
	}
	
}
