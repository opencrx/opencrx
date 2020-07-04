#!/bin/bash
ifconfig
apache2ctl restart
su -c "$@" opencrx
