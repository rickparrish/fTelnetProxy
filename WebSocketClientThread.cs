using RandM.RMLib;
using System;
using System.IO;
using System.Reflection;
using System.Text;
using System.Threading;

namespace RandM.fTelnetProxy {
    public class WebSocketClientThread : RMThread {
        private int _ConnectionId = 0;
        private WebSocketConnection _WebSocketConnection = null;

        public WebSocketClientThread(WebSocketConnection connection, int connectionId) {
            _WebSocketConnection = connection;
            _ConnectionId = connectionId;
        }

        protected override void Execute() {
            using (_WebSocketConnection) {
                // Defaults for redirect location
                string Hostname = Config.Default.TargetHostname;
                int Port = Config.Default.TargetPort;

                // Check if user is requesting a custom target
                if (_WebSocketConnection.Header["Path"] != "/") {
                    bool CanRelay = false;

                    // Check if program has relaying enabled
                    if (!string.IsNullOrEmpty(Config.Default.RelayFilename)) {
                        string[] HostAndPort = _WebSocketConnection.Header["Path"].Split('/');
                        if ((HostAndPort.Length == 3) && (int.TryParse(HostAndPort[2], out Port))) {
                            Hostname = HostAndPort[1];

                            try {
                                // Read relay file
                                if (File.Exists(Config.Default.RelayFilename)) {
                                    string[] AllowedHosts = File.ReadAllLines(Config.Default.RelayFilename);
                                    if (AllowedHosts.Length > 0) {
                                        // Check for a whitelisted port
                                        string[] AllowedPorts = AllowedHosts[0].Split(',');
                                        foreach (string AllowedPort in AllowedPorts) {
                                            if (AllowedPort == Port.ToString()) {
                                                CanRelay = true;
                                                break;
                                            }
                                        }

                                        // Not a whitelisted port, check for a whitelisted host
                                        if (!CanRelay) {
                                            string RequestedHostPort = Hostname + ":" + Port.ToString();
                                            foreach (string AllowedHost in AllowedHosts) {
                                                if (AllowedHost.Trim() == RequestedHostPort) {
                                                    CanRelay = true;
                                                    break;
                                                }
                                            }
                                        }
                                    } else {
                                        RMLog.Error("{" + _ConnectionId.ToString() + "} Relay file is empty: '" + Config.Default.RelayFilename + "'");
                                    }
                                } else {
                                    RMLog.Error("{" + _ConnectionId.ToString() + "} Relay file does not exist: '" + Config.Default.RelayFilename + "'");
                                }
                            } catch (Exception ex) {
                                RMLog.Exception(ex, "{" + _ConnectionId.ToString() + "} Error reading relay file: '" + Config.Default.RelayFilename + "'");
                            }
                        }
                    }

                    if (!CanRelay) {
                        RMLog.Info("{" + _ConnectionId.ToString() + "} Rejecting request for " + Hostname + ":" + Port.ToString());
                        _WebSocketConnection.WriteLn("Sorry, for security reasons this proxy won't connect to " + Hostname + ":" + Port.ToString());
                        Thread.Sleep(2500);
                        return;
                    }
                }

                // Try to connect to the desired Host and Port
                _WebSocketConnection.Write(Ansi.ClrScr() + "Connecting to " + Hostname + ":" + Port.ToString() + "...");
                using (TcpConnection _TcpConnection = new TcpConnection()) {
                    if (_TcpConnection.Connect(Hostname, Port)) {
                        RMLog.Info("{" + _ConnectionId.ToString() + "} Connected to " + Hostname + ":" + Port.ToString());
                        _WebSocketConnection.WriteLn("connected!");

                        // Repeatedly move data around until a connection is closed (or a stop is requested)
                        bool DoSleep = true;
                        while (!_Stop && _WebSocketConnection.Connected && _TcpConnection.Connected) {
                            DoSleep = true;

                            if (_WebSocketConnection.CanRead()) {
                                _TcpConnection.WriteBytes(_WebSocketConnection.ReadBytes());
                                DoSleep = false;
                            }

                            if (_TcpConnection.CanRead()) {
                                _WebSocketConnection.WriteBytes(_TcpConnection.ReadBytes());
                                DoSleep = false;
                            }

                            if (DoSleep) Thread.Sleep(1);
                        }

                        // Check why we exited the loop
                        if (_Stop) {
                            RMLog.Info("{" + _ConnectionId.ToString() + "} Stop requested");
                            _WebSocketConnection.Write(Ansi.GotoXY(1, 1) + Ansi.CursorDown(255) + "\r\nProxy server shutting down...");
                            Thread.Sleep(2500);
                        } else if (!_WebSocketConnection.Connected) {
                            RMLog.Info("{" + _ConnectionId.ToString() + "} Client closed connection");
                        } else if (!_TcpConnection.Connected) {
                            RMLog.Info("{" + _ConnectionId.ToString() + "} Server closed connection");
                            _WebSocketConnection.Write(Ansi.GotoXY(1, 1) + Ansi.CursorDown(255) + "\r\nServer closed connection...");
                            Thread.Sleep(2500);
                        }
                    } else {
                        RMLog.Info("{" + _ConnectionId.ToString() + "} Unable to connect to " + Hostname + ":" + Port.ToString());
                        _WebSocketConnection.WriteLn("unable to connect!");
                        Thread.Sleep(2500);
                    }
                }
            }
        }
    }
}
