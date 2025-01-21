/**
 * =======================================================================
 * = Description: openCRX/Core
 * = Name: build.gradle.kts
 * = Copyright:   the original authors.
 * =======================================================================
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
 * This product includes software developed by the Apache Software
 * Foundation (http://www.apache.org/).
 * 
 * This product includes software developed by contributors to
 * openMDX (http://www.openmdx.org/)
 */

import java.io.*
import java.util.*

 plugins {
    `kotlin-dsl`
    eclipse
}

dependencies {
    implementation(kotlin("stdlib"))	
    implementation(gradleApi())
    implementation(localGroovy())
}

repositories {
    mavenCentral()
}

val targetPlatform = JavaVersion.valueOf("VERSION_21")

eclipse {
	project {
    	name = "openCRX 6 ~ BuildSrc"
    }
}

tasks.register<Jar>("opencrx-buildSrc.jar") {
	dependsOn("classes")
	destinationDirectory.set(File("${rootDir}/jre-" + targetPlatform + "/" + project.getName() + "/lib"))
    archiveFileName.set("opencrx-buildSrc.jar")
    from(project.layout.buildDirectory.dir("classes/kotlin/main"))
    from(project.layout.buildDirectory.dir("resources/main"))
	doLast {
		ant.withGroovyBuilder {
			"jar"(
				"destfile" to File("../jre-" + targetPlatform + "/" + project.getName() + "/lib/opencrx-buildSrc-sources.jar")
			) {
				"fileset"(
					"dir" to "src/main/kotlin"
				)
			}
		}
	}
}

tasks.register("deliverables") {
	dependsOn("opencrx-buildSrc.jar")
}

tasks {
	assemble {
		dependsOn("deliverables")
	}
}
