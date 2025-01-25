/*
 * ====================================================================
 * Project:     openCRX/Gradle, http://www.opencrx.org/
 * Description: BpiWarTask
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
import org.gradle.api.file.FileTree
import org.gradle.api.file.RelativePath

open class BpiWarTask : ArchiveTask() {

	init {
   		destinationDirectory.set(File(deliverDir, "deployment-unit"))
	    archiveFileName.set(getWebAppName("bpi") + ".war")
	    duplicatesStrategy = DuplicatesStrategy.EXCLUDE
	    includeEmptyDirs = false
		manifest {
	        attributes(
	        	getManifest(
	        		"openCRX/Bpi " + providerName + " WAR",
	        		"opencrx-bpi-" + providerName + ".war"
	        	)
	        )
	    }
		var opencrxCoreConfigFiles: FileTree = project.zipTree(project.getConfigurations().getByName("opencrxCoreConfig").singleFile)
	    var nSegmentsDataDirGeneric = RelativePath(false, dataDirGeneric).segments.size
		// META-INF
		from(File(dataHome, dataDir + ".bpi/META-INF")) { into("META-INF"); filter { line -> archiveFilter(line) } }
		from(opencrxCoreConfigFiles) { include(dataDirGeneric + ".bpi/META-INF/"); eachFile { relativePath = RelativePath(true, *relativePath.segments.drop(nSegmentsDataDirGeneric).toTypedArray()) }; filter { line -> archiveFilter(line) } }
		// WEB-INF
		from(File(dataHome, dataDir + ".bpi/WEB-INF")) { into("WEB-INF"); include("web.xml", "*.xml"); exclude("*/*"); filter { line -> archiveFilter(line) } }
		from(opencrxCoreConfigFiles) { include(dataDirGeneric + ".bpi/WEB-INF/web.xml", dataDirGeneric + ".bpi/WEB-INF/*.xml"); exclude("*/*"); eachFile { relativePath = RelativePath(true, *relativePath.segments.drop(nSegmentsDataDirGeneric).toTypedArray()); filter { line -> archiveFilter(line) } } }
		// jsp
		from(File(dataHome, dataDir + ".bpi")) { include("*.jsp"); filter { line -> archiveFilter(line) } }
		from(opencrxCoreConfigFiles) { include(dataDirGeneric + ".bpi/*.jsp"); eachFile { relativePath = RelativePath(true, *relativePath.segments.drop(nSegmentsDataDirGeneric).toTypedArray()) }; filter { line -> archiveFilter(line) } }		
		// documents
		from(File(dataHome, dataDir + ".bpi/documents")) { into("documents") }
		from(opencrxCoreConfigFiles) { include(dataDirGeneric + ".bpi/documents/"); eachFile { relativePath = RelativePath(true, *relativePath.segments.drop(nSegmentsDataDirGeneric).toTypedArray()) } }		
	}

}
