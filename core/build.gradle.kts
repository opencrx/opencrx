/**
 * =======================================================================
 * = Description: openCRX/Core 
 * = Name: build.gradle.kts
 * = Copyright:   (c) 2020-2023 the original authors.
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

import org.gradle.kotlin.dsl.*
import org.w3c.dom.Element
import java.util.*
import java.io.*

plugins {
	java
	`java-library`
	eclipse
}

apply(plugin = "ear")
apply(plugin = "opencrx")

repositories {
	mavenCentral()
    maven {
        url = uri("https://www.openmdx.org/repos/releases")
    }
}

var env = Properties()
env.load(FileInputStream(File(project.getRootDir(), "build.properties")))
val targetPlatform = JavaVersion.valueOf(env.getProperty("target.platform"))

eclipse {
	project {
    	name = "openCRX 5 ~ Core"
    }
    jdt {
		sourceCompatibility = targetPlatform
    	targetCompatibility = targetPlatform
    	javaRuntimeName = "JavaSE-" + targetPlatform    	
    }    
}

fun getProjectImplementationVersion(): String {
	return project.getVersion().toString();
}

fun getDeliverDir(): File {
	return File(project.getRootDir(), "jre-" + targetPlatform + "/" + project.getName());
}

fun touch(file: File) {
	ant.withGroovyBuilder { "touch"("file" to file, "mkdirs" to true) }
}

val earlib by configurations
val tools by configurations
val opencrxCoreModels by configurations
val opencrxCoreConfig by configurations
val testRuntimeOnly by configurations
testRuntimeOnly.extendsFrom(earlib.copyRecursive())

// required for bootstrapping 
touch(File(File(getDeliverDir(), "lib"), "opencrx-core-config.jar"))

dependencies {
	opencrxCoreConfig(fileTree(File(getDeliverDir(), "lib")) { include("opencrx-core-config.jar"); } )
	earlib(fileTree(File(getDeliverDir(), "lib")) { include("*.jar"); exclude("opencrx-client.jar", "opencrx-core-config.jar" ) } )
	tools(files(layout.buildDirectory.dir("classes/java/main")))
	tools(files(layout.buildDirectory.dir("resources/main")))
	tools(files(layout.buildDirectory.dir("generated/sources/model/opencrx-core.openmdx-xmi.zip")))
	// test
    testImplementation("org.junit.jupiter:junit-jupiter:5.10.0")
    // testRuntimeOnly
    testRuntimeOnly(fileTree(File(getDeliverDir(), "lib")) { include("*.jar"); exclude("opencrx-client.jar", "opencrx-core-config.jar", "opencrx-config-crx.jar" ) } )
    testRuntimeOnly("org.junit.jupiter:junit-jupiter:5.10.0")
    testRuntimeOnly("org.xerial:sqlite-jdbc:3.34.+")
	testRuntimeOnly("com.atomikos:transactions-jta:6.0.0")
	testRuntimeOnly("com.atomikos:transactions-jdbc:6.0.0")
    testRuntimeOnly("org.postgresql:postgresql:42.6.0")
}

sourceSets {
    main {
        java {
            srcDir("src/main/java")
            srcDir("src/data/org.opencrx/WEB-INF/classes")
            srcDir(layout.buildDirectory.dir("generated/sources/java/main"))
        }
    }
    test {
        java {
            srcDir("src/test/java")
            srcDir(layout.buildDirectory.dir("generated/sources/java/test"))
        }
        resources {
        	srcDir("src/test/resources")
        }
    }
}

sourceSets.create("sample") {
    java.srcDir("src/sample/java")
    resources.srcDir("src/sample/resources")
}

tasks.test {
    useJUnitPlatform()
    maxHeapSize = "4G"
}

tasks {
	assemble {
		dependsOn("opencrx.ear")
	}
}

tasks.withType<JavaCompile> {
	dependsOn("generate-model")
}

tasks.register("deliverables") {
	dependsOn("opencrx-core.jar", "opencrx-core-config.jar", "opencrx-client.jar")
}

tasks.named<Ear>("ear") {
    dependsOn("opencrx.ear")
}
