/**
 * =======================================================================
 * = Description: openCRX/Core 
 * = Name: build.gradle.kts
 * = Copyright:   (c) 2020 CRIXP AG
 * =======================================================================
 * This software is published under the BSD license
 * as listed below.
 * 
 * Copyright (c) 2020, CRIXP Corp., Switzerland
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
	war
	distribution
}

apply(plugin = "ear")
apply(plugin = "opencrx")

repositories {
	mavenCentral()
	jcenter()
    maven {
        url = uri("https://www.openmdx.org/repos/releases")
    }
}

var env = Properties()
env.load(FileInputStream(File(project.getRootDir(), "build.properties")))
val targetPlatform = JavaVersion.valueOf(env.getProperty("target.platform"))

fun setJreContainerOptions(el: Element) {
    fun Element.firstElement(predicate: (Element.() -> Boolean)) =
        childNodes
            .run { (0 until length).map(::item) }
            .filterIsInstance<Element>()
            .first { it.predicate() }
    var jreContainerEl = el
        .firstElement {
            tagName == "classpathentry"
            && getAttribute("kind") == "con"
            && getAttribute("path").startsWith("org.eclipse.jdt.launching.JRE_CONTAINER/org.eclipse.jdt.internal.debug.ui.launcher.StandardVMType")
        }		
    var xmlDoc = el.getOwnerDocument()
    var addExportsEl = xmlDoc.createElement("attribute")
    addExportsEl.setAttribute("name", "add-exports")
    addExportsEl.setAttribute("value", "java.naming/com.sun.jndi.ldap=ALL-UNNAMED")
    var moduleEl = xmlDoc.createElement("attribute")
    moduleEl.setAttribute("name", "module")
    moduleEl.setAttribute("value", "true")
    var attributesEl = xmlDoc.createElement("attributes")
    attributesEl.appendChild(addExportsEl)
    attributesEl.appendChild(moduleEl)
    jreContainerEl.appendChild(attributesEl)
}

eclipse {
	project {
    	name = "openCRX 5 ~ Core"
    }
    classpath {
    	file {
            withXml { setJreContainerOptions(asElement()) }
    	}
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

// required for bootstrapping 
touch(File(File(getDeliverDir(), "lib"), "opencrx-core-config.jar"))

dependencies {
	opencrxCoreConfig(fileTree(File(getDeliverDir(), "lib")) { include("opencrx-core-config.jar"); } )
	earlib(fileTree(File(getDeliverDir(), "lib")) { include("*.jar"); exclude("opencrx-client.jar", "opencrx-core-config.jar" ) } )
	tools(files("$buildDir/classes/java/main"))
	tools(files("$buildDir/resources/main"))
	tools(files("$buildDir/generated/sources/model/opencrx-core.openmdx-xmi.zip"))
	// test
    testImplementation("org.junit.jupiter:junit-jupiter:5.6.0")
    testRuntimeOnly(earlib)
    testRuntimeOnly("org.junit.jupiter:junit-jupiter:5.6.0")
    testRuntimeOnly("org.xerial:sqlite-jdbc:3.34.+")
}

sourceSets {
    main {
        java {
            srcDir("src/main/java")
            srcDir("src/data/org.opencrx/WEB-INF/classes")
            srcDir("$buildDir/generated/sources/java/main")
        }
    }
    test {
        java {
            srcDir("src/test/java")
            srcDir("$buildDir/generated/sources/java/test")
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
	options.compilerArgs = listOf("--add-exports", "java.naming/com.sun.jndi.ldap=ALL-UNNAMED")
}

tasks.register("deliverables") {
	dependsOn("opencrx-core.jar", "opencrx-core-config.jar", "opencrx-client.jar")
}

distributions {
    main {
    	distributionBaseName.set("opencrx-" + getProjectImplementationVersion() + "-core-jre-" + targetPlatform)
        contents {
        	// core
        	from(".") { into("core"); include("LICENSE", "*.LICENSE", "NOTICE", "*.properties", "build*.*", "*.xml", "*.kts") }
            from("src") { into("core/src") }
        	// gradle
        	from("../gradle") { into("gradle"); include("LICENSE", "*.LICENSE", "NOTICE", "*.properties", "build*.*", "*.xml", "*.kts") }
            from("../gradle/src") { into("gradle/src") }
            // etc
            from("etc") { into("core/etc") }
            // rootDir
            from("..") { include("*.properties", "*.kts" ) }
            // jre-...
            from("../jre-" + targetPlatform + "/core/lib") { into("jre-" + targetPlatform + "/core/lib") }
            from("../jre-" + targetPlatform + "/gradle/repo") { into("jre-" + targetPlatform + "/gradle/repo") }
        }
    }
}
