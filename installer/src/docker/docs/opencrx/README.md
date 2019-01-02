# What is openCRX

openCRX is an open source CRM solution that meets the needs of organizations requiring multifunctional, enterprise-wide coordination of sales generation, sales fulfillment, marketing and service activities to customers, partners, suppliers or intermediaries.

> [opencrx.org](http://www.opencrx.org)

![logo](https://raw.githubusercontent.com/docker-library/docs/master/opencrx/logo.png)

# How to use this image

## Build this image

    docker build -t opencrx4:latest .
    
## Run this image

    docker run -d -p 8080:80 -p 8009:8009 -p 8001:8001 --name opencrx4 opencrx4:latest
    
## Stop container

    docker stop opencrx4
    
## Start container

    docker start opencrx4
    
## Inspect logs

    docker logs -f opencrx4
    docker exec -i opencrx4 cat /root/opt/opencrx/apache-tomee-plus-7.0.5/logs/catalina.yyyy-mm-dd.log
    docker cp opencrx4:/root/opt/opencrx/apache-tomee-plus-7.0.5/logs/catalina.yyyy-mm-dd.log .

# How to extend this image

Here is an example of a custom Dockerfile which replaces the default tomee.xml by a custom configuration:

    FROM opencrx4:latest
    MAINTAINER demo@opencrx.org
    COPY ./tomee.xml $HOME/opt/opencrx/apache-tomee-plus-7.0.5/conf/
