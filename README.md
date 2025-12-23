fTelnetProxy
============

A WebSocket to TCP proxy for <a href="https://github.com/rickparrish/fTelnet">fTelnet</a>

If you want to build from source you'll need to download the RMLib project from here:<br />
https://github.com/rickparrish/RMLib

If you don't want to build from source but do want to run your own local instance, then you can download the two
required binaries from here:<br />
https://github.com/rickparrish/fTelnetProxy/raw/refs/heads/master/fTelnetProxy/bin/Release/fTelnetProxy.exe<br />
https://github.com/rickparrish/fTelnetProxy/raw/refs/heads/master/fTelnetProxy/bin/Release/RMLib.dll

And if you want the easiest solution, then I run a few public proxies that you can use with fTelnet here:<br />
http://proxy.ftelnet.ca<br />
As well as an embed wizard that will simplify fTelnet and proxy configuration here:<br />
http://embed-v2.ftelnet.ca

---

If you're running your own instance...

- On Windows, just make sure you have the .NET Framework 4.6.2 installed

- On Linux, I just install `mono-complete` on my Ubuntu 24.04 64-bit VPSes (mono-complete is probably overkill, but it works)
  
- fTelnetProxy.ini will be created with default values the first time you run fTelnetProxy.exe.  You may override settings from the .ini
by passing command-line parameters.  Use fTelnetProxy.exe /? to list the available parameters.

- To install or uninstall as a Windows Service, use the /i or /u parameters.