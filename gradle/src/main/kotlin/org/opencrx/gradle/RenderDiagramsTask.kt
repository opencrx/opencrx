/*
 * ====================================================================
 * Project:     openCRX/Gradle, http://www.opencrx.org/
 * Description: RenderDiagramsTask
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
import org.gradle.api.tasks.InputFiles
import java.io.File

open class RenderDiagramsTask : ExecTask() {

	var albumPath = File(project.getBuildDir(), "generated/sources/model/diagrams/" + projectVendorName + " " + projectImplementationVersion + " Model")

	var albumExec: File
	init {
		val v = env.getProperty("albumExec");
		albumExec = if(v == null) File("!!!albumExec!!!") else File(v);		
	}

	var dotExec: File
	init {
		val v = env.getProperty("dotExec");
		dotExec = if(v == null) File("!!!dotExec!!!") else File(v);		
	}

	init {
		mainClass.set("org.openmdx.base.mof.spi.Model_1DiagramDrawer")
		args = listOf(
			"src/model/graphviz/diagrams",
			File(project.getBuildDir(), "generated/sources/model/diagrams/dot").toString()
		)
		doLast {
			getProject().copy { from(File(project.getBuildDir(), "generated/sources/model/diagrams/dot")); into(albumPath) }
			getProject().fileTree(albumPath).matching { include("**/*.dot") }.forEach { f ->
				getProject().exec {
					workingDir(f.parentFile)
					commandLine(dotExec, "-Tpng", "-o", f.name.replace(".dot", ".png"), f.name)
				}
				f.delete()
			}
			getProject().exec {
				workingDir(albumPath)
				commandLine(albumExec, "-theme", project.property("album.theme.name"), "-theme_path", project.property("album.theme.path"), "-theme_url", getProject().property("album.theme.url"), "-save_conf=off", "-clean=on")
			}
		}
	}

}
