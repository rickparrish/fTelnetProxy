using RandM.RMLib;
using System;
using System.Collections.Generic;
using System.IO;
using System.Net.Sockets;
using System.Text;
using System.Threading;

namespace RandM.fTelnetProxy {
    public class WebSocketServerThread : RMThread {
        private string _Address;
        private int _ClientThreadCounter = 0;
        private List<WebSocketClientThread> _ClientThreads = new List<WebSocketClientThread>();
        private object _ClientThreadsLock = new object();
        private int _Port;
        private WebSocketConnection _Server;

        public WebSocketServerThread(string address, int port) {
            _Address = address;
            _Port = port;
        }

        public int ClientConnectionCount {
            get {
                lock (_ClientThreadsLock) {
                    return _ClientThreads.Count;
                }
            }
        }

        void ClientThread_FinishEvent(object sender, EventArgs e) {
            if (sender is WebSocketClientThread) {
                lock (_ClientThreadsLock) {
                    if (_ClientThreads.Contains((WebSocketClientThread)sender)) {
                        _ClientThreads.Remove((WebSocketClientThread)sender);
                        RMLog.Info(_ClientThreads.Count.ToString() + " active connections");
                    } else {
                        RMLog.Error("ClientThread_FinishEvent did not find sender in _ClientThreads (sender=" + sender.ToString() + ")");
                    }
                }
            } else {
                RMLog.Error("ClientThread_FinishEvent's sender is not a WebSocketClientThread (sender=" + sender.ToString() + ")");
            }
        }

        protected override void Execute() {
            using (_Server = new WebSocketConnection()) {
                if (_Server.Listen(_Address, _Port)) {
                    while (!_Stop) {
                        try {
                            // Accept an incoming connection
                            if (_Server.CanAccept(1000)) // 1 second
                            {
                                Socket NewSocket = _Server.Accept();
                                if (NewSocket != null) {
                                    lock (_ClientThreadsLock) {
                                        WebSocketClientThread ClientThread = new WebSocketClientThread(NewSocket, ++_ClientThreadCounter);
                                        ClientThread.FinishEvent += ClientThread_FinishEvent;
                                        _ClientThreads.Add(ClientThread);
                                        RMLog.Info(_ClientThreads.Count.ToString() + " active connections");
                                        ClientThread.Start();
                                    }
                                }
                            }
                        } catch (Exception ex) {
                            RMLog.Exception(ex, "Unable to accept new websocket connection");
                        }
                    }

                    // Stop client threads
                    int ClientThreadCount = 0;
                    lock (_ClientThreadsLock) {
                        foreach (var ClientThread in _ClientThreads) {
                            if (ClientThread != null) ClientThread.Stop();
                        }
                        ClientThreadCount = _ClientThreads.Count;
                    }

                    // Wait for client threads
                    while (ClientThreadCount > 0) {
                        lock (_ClientThreadsLock) {
                            ClientThreadCount = _ClientThreads.Count;
                        }
                        Thread.Sleep(100);
                    }
                } else {
                    RMLog.Error("WebSocket Server Thread: Unable to listen on " + _Address + ":" + _Port);
                }
            }
        }

        public override void Stop() {
            // Close the socket so that any waits on ReadLn(), ReadChar(), etc, will not block
            if (_Server != null) _Server.Close();

            base.Stop();
        }
    }
}
