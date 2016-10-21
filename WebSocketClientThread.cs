using RandM.RMLib;
using System;
using System.IO;
using System.Reflection;
using System.Text;
using System.Threading;
using System.Net.Sockets;

namespace RandM.fTelnetProxy {
    public class WebSocketClientThread : RMThread {
        private int _ConnectionId = 0;
        private Socket _Socket = null;

        public WebSocketClientThread(Socket socket, int connectionId) {
            _Socket = socket;
            _ConnectionId = connectionId;
        }

        protected override void Dispose(bool disposing) {
            if (!_Disposed) {
                if (disposing) {
                    // dispose managed state (managed objects).
                    if (_Socket != null) {
                        try {
                            _Socket.Shutdown(SocketShutdown.Both);
                        } catch {
                            // Ignore on dispose
                        }
                        try {
                            _Socket.Close();
                        } catch {
                            // Ignore on dispose
                        }
                        _Socket = null;
                    }
                }

                // free unmanaged resources (unmanaged objects)
                // set large fields to null.

                // Call the base dispose
                base.Dispose(disposing);
            }
        }

        protected override void Execute() {
            try {
                // Handle non-proxy connections
                WebSocketConnection NewConnection = new WebSocketConnection(true, Config.Default.Certificate);
                if (NewConnection.Open(_Socket)) {
                    RMLog.Debug("{" + _ConnectionId.ToString() + "} Opened connection from " + NewConnection.GetRemoteIP() + ":" + NewConnection.GetRemotePort());
                    if (NewConnection.Header["Path"] == "/ping") {
                        // Handle ping requests (from proxy.ftelnet.ca most likely)
                        string Ping = NewConnection.ReadLn(1000);
                        if (NewConnection.ReadTimedOut) {
                            RMLog.Debug("Answering a /ping (no time received) from " + NewConnection.GetRemoteIP() + ":" + NewConnection.GetRemotePort());
                        } else {
                            RMLog.Debug("Answering a /ping (" + Ping + ") from " + NewConnection.GetRemoteIP() + ":" + NewConnection.GetRemotePort());
                            NewConnection.Write(Ping);
                        }
                        return;
                    }
                } else {
                    if (NewConnection.FlashPolicyFileRequest) {
                        RMLog.Info("{" + _ConnectionId.ToString() + "} Answered flash policy file request from " + NewConnection.GetRemoteIP() + ":" + NewConnection.GetRemotePort().ToString());
                    } else {
                        RMLog.Debug("{" + _ConnectionId.ToString() + "} Invalid WebSocket connection from " + NewConnection.GetRemoteIP() + ":" + NewConnection.GetRemotePort().ToString());
                    }
                    return;
                }

                // If we get here it's a proxy connection, so handle it
                RMLog.Info("{" + _ConnectionId.ToString() + "} Connection accepted from " + NewConnection.GetRemoteIP() + ":" + NewConnection.GetRemotePort());

                string MessageText = string.Format("{0}\t{1}\t{2}\t{3}\t{4}\t{5}\r\n",
                    DateTime.Now.ToString(),
                    NewConnection.GetRemoteIP(),
                    NewConnection.GetRemotePort(),
                    NewConnection.Header["Path"],
                    NewConnection.Protocol,
                    NewConnection.SubProtocol);
                FileUtils.FileAppendAllText(Path.Combine(ProcessUtils.StartupPath, "fTelnetProxy-Connections.log"), MessageText, Encoding.ASCII);

                // Defaults for redirect location
                string Hostname = Config.Default.TargetHostname;
                int Port = Config.Default.TargetPort;

                // Check if user is requesting a custom target
                if (NewConnection.Header["Path"] != "/") {
                    bool CanRelay = false;

                    // Extract the requested host and port
                    string[] HostAndPort = NewConnection.Header["Path"].Split('/');
                    if ((HostAndPort.Length == 3) && (int.TryParse(HostAndPort[2], out Port))) {
                        Hostname = HostAndPort[1];
                        if (Config.Default.TargetHostname.ToLower().Trim() == Hostname.ToLower().Trim()) {
                            // User is requesting the target defined by the proxy admin, so check if it's to an allowed port
                            CanRelay = ((Port > 0) && (Port == Config.Default.TargetPort) || (Port == Config.Default.RLoginPort));
                        } else if (!string.IsNullOrEmpty(Config.Default.RelayFilename)) {
                            // proxy admin has relaying enabled, so check against the relay.cfg file
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
                                            string RequestedHostPort = Hostname.ToLower() + ":" + Port.ToString();
                                            foreach (string AllowedHost in AllowedHosts) {
                                                if (AllowedHost.Trim().ToLower() == RequestedHostPort) {
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
                        NewConnection.WriteLn("Sorry, for security reasons this proxy won't connect to " + Hostname + ":" + Port.ToString());
                        Thread.Sleep(2500);
                        return;
                    }
                }

                // Try to connect to the desired Host and Port
                NewConnection.Write(Ansi.ClrScr() + "Connecting to " + Hostname + ":" + Port.ToString() + "...");
                using (TcpConnection _TcpConnection = new TcpConnection()) {
                    if (_TcpConnection.Connect(Hostname, Port)) {
                        RMLog.Info("{" + _ConnectionId.ToString() + "} Connected to " + Hostname + ":" + Port.ToString());
                        NewConnection.WriteLn("connected!");

                        // Repeatedly move data around until a connection is closed (or a stop is requested)
                        bool DoSleep = true;
                        while (!_Stop && NewConnection.Connected && _TcpConnection.Connected) {
                            DoSleep = true;

                            if (NewConnection.CanRead()) {
                                _TcpConnection.WriteBytes(NewConnection.ReadBytes());
                                DoSleep = false;
                            }

                            if (_TcpConnection.CanRead()) {
                                NewConnection.WriteBytes(_TcpConnection.ReadBytes());
                                DoSleep = false;
                            }

                            if (DoSleep) Thread.Sleep(1);
                        }

                        // Check why we exited the loop
                        if (_Stop) {
                            RMLog.Info("{" + _ConnectionId.ToString() + "} Stop requested");
                            NewConnection.Write(Ansi.GotoXY(1, 1) + Ansi.CursorDown(255) + "\r\nProxy server shutting down...");
                            Thread.Sleep(2500);
                        } else if (!NewConnection.Connected) {
                            RMLog.Info("{" + _ConnectionId.ToString() + "} Client closed connection");
                        } else if (!_TcpConnection.Connected) {
                            RMLog.Info("{" + _ConnectionId.ToString() + "} Server closed connection");
                            NewConnection.Write(Ansi.GotoXY(1, 1) + Ansi.CursorDown(255) + "\r\nServer closed connection...");
                            Thread.Sleep(2500);
                        }
                    } else {
                        RMLog.Info("{" + _ConnectionId.ToString() + "} Unable to connect to " + Hostname + ":" + Port.ToString());
                        NewConnection.WriteLn("unable to connect!");
                        Thread.Sleep(2500);
                    }
                }
            } catch (Exception ex) {
                RMLog.Exception(ex, "{" + _ConnectionId.ToString() + "} Exception in client thread");
            }
        }
    }
}
