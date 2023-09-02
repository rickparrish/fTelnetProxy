# fTelnetProxy

A WebSocket to TCP proxy for <a href="https://github.com/rickparrish/fTelnet">fTelnet</a>

Don't want to go through the hassle of setting up your own proxy?
<a href="http://proxy.ftelnet.ca">I run a few public proxies you can use with fTelnet</a>

If you do want to go ahead with your own local install, download the contents of bin/Release:<br />
https://github.com/rickparrish/fTelnetProxy/blob/master/bin/Release/fTelnetProxy.exe?raw=true<br />
https://github.com/rickparrish/fTelnetProxy/blob/master/bin/Release/RMLib.dll?raw=true

On Windows, just make sure you have the .NET Framework 3.5 installed, and you should be good to go.

On Linux, I've tested (and run all my public proxies) with Ubuntu Server 14.04.  Unfortunately it
currently includes Mono 3.2.8 in the repositories, and there is a bug that prevents wss:// connections
from working with clients that support TLS 1.1 or 1.2 (so all modern clients) that wasn't fixed until 3.4.0, so:

  - If you need wss:// support, and 3.4.0 isn't available in the repositories yet, then 
<a href="http://www.mono-project.com/docs/compiling-mono/linux/#building-mono-from-a-release-package">Build Mono from a Release Package</a>.

  - If you don't care about wss:// support, then you can just apt-get install mono-runtime libmono-system-runtime2.0-cil
  
fTelnetProxy.ini will be created with default values the first time you run fTelnetProxy.exe.  You may override settings from the .ini
by passing command-line parameters.  Use fTelnetProxy.exe /? to list the available parameters.

To install or uninstall as a Windows Service, use the /i or /u parameters.

## Docker

You can also run fTelnetProxy in a Docker container.  The Dockerfile is included in the repository.
Recommended approach is to use the docker composer file to build and run the container.

Simply launch it using `docker compose up -d`, you can inspect the logs using `docker compose logs -f`

On first run, the container will create a default fTelnetProxy.ini file in the /app/bin/Release directory.
Modify it to your need.

You then use nginx (or apache) to proxy the connection to the container. 
Included is a ReverseProxySample.conf file, which assumes fteplnetproxy is routing on port 4101.

You should have the index.html in `/var/www/myproxy/index.html` and the ReverseProxySample.conf in `/etc/nginx/sites-available/myproxy.conf`

Then you can enable the site with `sudo ln -s /etc/nginx/sites-available/myproxy.conf /etc/nginx/sites-enabled/myproxy.conf`

And reload nginx (or apache) with `sudo nginx -s reload`

You can now access your website on port 443 and it will be proxied to the container, WSS will work as well.