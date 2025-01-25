/*
 * ====================================================================
 * Project:     openCRX/Gradle, http://www.opencrx.org/
 * Description: PortalWarTask
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
import org.gradle.api.file.DuplicatesStrategy
import org.gradle.api.file.FileTree
import org.gradle.api.file.RelativePath
import java.io.File

open class PortalWarTask: ArchiveTask() {

	init {
   		destinationDirectory.set(File(deliverDir, "deployment-unit"))
	    archiveFileName.set(getWebAppName("core") + ".war")
		manifest {
	        attributes(
	        	getManifest(
	        		"openCRX/Core " + providerName + " WAR",
	        		"opencrx-core-" + providerName + ".war"
	        	)
	        )
	    }
	    duplicatesStrategy = DuplicatesStrategy.EXCLUDE
	    includeEmptyDirs = false
		var opencrxCoreConfigFiles = project.zipTree(project.getConfigurations().getByName("opencrxCoreConfig").singleFile)
	    var openmdxInspectorFiles: FileTree = project.zipTree(project.getConfigurations().getByName("openmdxInspector").singleFile)
	    var nSegmentsDataDirGeneric = RelativePath(false, dataDirGeneric).segments.size
	    var webInfConfigPath = RelativePath(false, *"WEB-INF/config".split("/").toTypedArray());
		// WEB-INF
		from(File(dataHome, dataDir + "/WEB-INF")) { into("WEB-INF"); exclude("**/*.java"); filter { line -> archiveFilter(line) } }
		from(opencrxCoreConfigFiles) { include(dataDirGeneric + "/WEB-INF/**"); exclude("**/*.java"); exclude("*/*"); eachFile { relativePath = RelativePath(true, *relativePath.segments.drop(nSegmentsDataDirGeneric).toTypedArray()) }; filter { line -> archiveFilter(line) } }
		// META-INF
		from(File(dataHome, dataDir + "/META-INF")) { into("META-INF"); filter { line -> archiveFilter(line) } }
		from(opencrxCoreConfigFiles) { include(dataDirGeneric + "/META-INF/"); eachFile { relativePath = RelativePath(true, *relativePath.segments.drop(nSegmentsDataDirGeneric).toTypedArray()) }; filter { line -> archiveFilter(line) } }
	    // WEB-INF/classes
		from(project.layout.buildDirectory.dir("classes/java/main")) { include(dataDir.replace(".","/") + "/portal/wizard/**"); into("WEB-INF/classes") }
		from(project.layout.buildDirectory.dir("classes/java/main")) { include(dataDirGeneric.replace(".","/") + "/portal/wizard/**"); into("WEB-INF/classes") }
		// WEB-INF/code
		from(File(dataHome, dataDir + "/code")) { into("WEB-INF/config/code"); filter { line -> archiveFilter(line) } }
		from(opencrxCoreConfigFiles) { include(dataDirGeneric + "/code/"); eachFile { relativePath = webInfConfigPath.append(RelativePath(true, *relativePath.segments.drop(nSegmentsDataDirGeneric).toTypedArray())) }; filter { line -> archiveFilter(line) } }
	 	// WEB-INF/bootstrap
		from(File(dataHome, dataDir + "/bootstrap")) { into("WEB-INF/config/bootstrap"); filter { line -> archiveFilter(line) } }
		from(opencrxCoreConfigFiles) { include(dataDirGeneric + "/bootstrap/"); eachFile { relativePath = webInfConfigPath.append(RelativePath(true, *relativePath.segments.drop(nSegmentsDataDirGeneric).toTypedArray())) }; filter { line -> archiveFilter(line) } }
		// WEB-INF/data
		from(File(dataHome, dataDir + "/data")) { into("WEB-INF/config/data"); filter { line -> archiveFilter(line) } }
		from(opencrxCoreConfigFiles) { include(dataDirGeneric + "/data/"); eachFile { relativePath = webInfConfigPath.append(RelativePath(true, *relativePath.segments.drop(nSegmentsDataDirGeneric).toTypedArray())) }; filter { line -> archiveFilter(line) } }
		// WEB-INF/filters
		from(File(dataHome, dataDir + "/filters")) { into("WEB-INF/config/filters"); filter { line -> archiveFilter(line) } }
		from(opencrxCoreConfigFiles) { include(dataDirGeneric + "/filters/"); eachFile { relativePath = webInfConfigPath.append(RelativePath(true, *relativePath.segments.drop(nSegmentsDataDirGeneric).toTypedArray())) }; filter { line -> archiveFilter(line) } }
		// WEB-INF/scripts
		from(File(dataHome, dataDir + "/scripts")) { into("WEB-INF/config/scripts"); filter { line -> archiveFilter(line) } }
		from(opencrxCoreConfigFiles) { include(dataDirGeneric + "/scripts/"); eachFile { relativePath = webInfConfigPath.append(RelativePath(true, *relativePath.segments.drop(nSegmentsDataDirGeneric).toTypedArray())) }; filter { line -> archiveFilter(line) } }
		// WEB-INF/texts
		from(File(dataHome, dataDir + "/texts")) { into("WEB-INF/config/texts"); filter { line -> archiveFilter(line) } }
		from(opencrxCoreConfigFiles) { include(dataDirGeneric + "/texts/"); eachFile { relativePath = webInfConfigPath.append(RelativePath(true, *relativePath.segments.drop(nSegmentsDataDirGeneric).toTypedArray())) }; filter { line -> archiveFilter(line) } }
		// WEB-INF/ui
		from(File(dataHome, dataDir + "/ui")) { into("WEB-INF/config/ui"); filter { line -> archiveFilter(line) } }
		from(opencrxCoreConfigFiles) { include(dataDirGeneric + "/ui/"); eachFile { relativePath = webInfConfigPath.append(RelativePath(true, *relativePath.segments.drop(nSegmentsDataDirGeneric).toTypedArray())) }; filter { line -> archiveFilter(line) } }
		// WEB-INF/control
		from(File(dataHome, dataDir + "/control")) { into("WEB-INF/config/control"); filter { line -> archiveFilter(line) } }
		from(opencrxCoreConfigFiles) { include(dataDirGeneric + "/control/"); eachFile { relativePath = webInfConfigPath.append(RelativePath(true, *relativePath.segments.drop(nSegmentsDataDirGeneric).toTypedArray())) }; filter { line -> archiveFilter(line) } }
		// _style
		from(File(dataHome, dataDir + "/_style")) { into("_style"); filter { line -> archiveFilter(line) } }
		from(opencrxCoreConfigFiles) { include(dataDirGeneric + "/_style/"); eachFile { relativePath = RelativePath(true, *relativePath.segments.drop(nSegmentsDataDirGeneric).toTypedArray()) }; filter { line -> archiveFilter(line) } }
		// js
		from(File(dataHome, dataDir + "/js")) { into("js"); }
		from(opencrxCoreConfigFiles) { include(dataDirGeneric + "/js/"); eachFile { relativePath = RelativePath(true, *relativePath.segments.drop(nSegmentsDataDirGeneric).toTypedArray()) } }
		// html
		from(File(dataHome, dataDir + "/html")) { filter { line -> archiveFilter(line) } }
		from(opencrxCoreConfigFiles) { include(dataDirGeneric + "/html/"); eachFile { relativePath = RelativePath(true, *relativePath.segments.drop(nSegmentsDataDirGeneric + 1).toTypedArray()) }; filter { line -> archiveFilter(line) } }
		// jsp, properties
		from(File(dataHome, dataDir)) { include("**/*.jsp", "**/*.properties"); exclude("**/*.texts.properties"); filter { line -> archiveFilter(line) } }
		from(opencrxCoreConfigFiles) { include(dataDirGeneric + "/**/*.jsp", dataDirGeneric + "/**/*.properties"); exclude("**/*.texts.properties"); eachFile { relativePath = RelativePath(true, *relativePath.segments.drop(nSegmentsDataDirGeneric).toTypedArray()) }; filter { line -> archiveFilter(line) } }
		// images
		from(File(dataHome, dataDir + "/images")) { into("images"); include("*.gif", "*.jpg", "*.png", "*.ico", "*.svg") }
		from(opencrxCoreConfigFiles) { include(dataDirGeneric + "/images/*.gif", dataDirGeneric + "/images/*.jpg", dataDirGeneric + "/images/*.png", dataDirGeneric + "/images/*.ico", dataDirGeneric + "/images/*.svg"); eachFile { relativePath = RelativePath(true, *relativePath.segments.drop(nSegmentsDataDirGeneric).toTypedArray()) } }
		// documents
		from(File(dataHome, dataDir + "/documents")) { into("documents") }
		from(opencrxCoreConfigFiles) { include(dataDirGeneric + "/documents/"); eachFile { relativePath = RelativePath(true, *relativePath.segments.drop(nSegmentsDataDirGeneric).toTypedArray()) } }
		// wizards
		from(File(dataHome, dataDir + "/wizards")) { into("wizards"); filter { line -> archiveFilter(line) } }
		from(opencrxCoreConfigFiles) { include(dataDirGeneric + "/wizards/"); eachFile { relativePath = RelativePath(true, *relativePath.segments.drop(nSegmentsDataDirGeneric).toTypedArray()) }; filter { line -> archiveFilter(line) } }
		// openmdx-inspector
		from(openmdxInspectorFiles) { exclude("**/texts/de_DE/**", "images/**"); filter { line -> archiveFilter(line) } }
		from(openmdxInspectorFiles) { include("images/**") }
	}
	
}
